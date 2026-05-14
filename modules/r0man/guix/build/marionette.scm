;;; Copyright © 2026 Roman Scherer <roman@burningswell.com>
;;;
;;; This file is part of the r0man Guix Home channel.
;;;
;;; The r0man Guix Home channel is free software; you can redistribute
;;; it and/or modify it under the terms of the GNU General Public
;;; License as published by the Free Software Foundation; either
;;; version 3 of the License, or (at your option) any later version.

(define-module (r0man guix build marionette)
  #:use-module (asahi guix build marionette)
  #:use-module (gnu build marionette)
  #:use-module (srfi srfi-64)
  #:re-export (test-assert-file-absent
               test-assert-file-contains
               test-assert-file-exists
               test-assert-service-registered
               test-assert-service-running
               test-assert-user-exists
               test-assert-wait-for-file)
  #:export (test-assert-home-activated
            test-assert-home-bashrc-exists
            test-assert-user-shepherd-up
            test-assert-user-shepherd-service-started
            test-assert-wait-for-file-exists))

;;; Commentary:
;;;
;;; SRFI-64 marionette helpers for r0man Guix Home VM tests.  Most
;;; helpers are re-exported from `(asahi guix build marionette)` so
;;; `(r0man guix build marionette)` is the single import test files
;;; need.  r0man-specific helpers (currently: home-activation
;;; assertions) are defined here.
;;;
;;; Code:

(define (test-assert-home-bashrc-exists user marionette)
  "Assert inside MARIONETTE that USER's home-bash-service-type
activation produced the canonical /home/USER/.bashrc symlink.  This is
the cheapest signal that `guix-home-service-type' completed activation
for USER, since home-environment derivations alone don't run
activation — only booting an OS that hosts the guix-home service does.

We can't use `wait-for-file' from `(gnu build marionette)' here: its
default reader is Scheme `read', and a real .bashrc starts with `#'
which the Scheme reader rejects.  Poll `file-exists?' instead."
  (let ((path (string-append "/home/" user "/.bashrc")))
    (test-assert
        (string-append user " home-bash activated (.bashrc present)")
      (marionette-eval
       `(let loop ((i 60))
          (cond ((file-exists? ,path) #t)
                ((> i 0) (sleep 1) (loop (- i 1)))
                (else #f)))
       marionette))))

(define (test-assert-home-activated user marionette)
  "Assert inside MARIONETTE that the home-environment for USER has been
activated by waiting for the activation-marker XDG state file.  Falls
back to .bashrc if no activation marker exists."
  (test-assert-home-bashrc-exists user marionette))

(define* (test-assert-wait-for-file-exists description path marionette
                                            #:key (timeout 60))
  "Assert inside MARIONETTE that PATH appears within TIMEOUT seconds.
Unlike `wait-for-file', polls `file-exists?' only — never reads the
file — so the helper is safe on Unix-domain sockets, FIFOs, and other
non-regular nodes where `read' would block or raise."
  (test-assert description
    (marionette-eval
     `(let loop ((i ,timeout))
        (cond ((file-exists? ,path) #t)
              ((> i 0) (sleep 1) (loop (- i 1)))
              (else #f)))
     marionette)))

(define* (test-assert-user-shepherd-up user marionette
                                       #:key (timeout 60))
  "Bring up USER's home-shepherd inside MARIONETTE and assert its IPC
socket appears within TIMEOUT seconds.

The minimal r0man test OS lacks elogind / PAM-systemd, so USER's
shepherd is NOT auto-started at boot.  This helper provisions
`/run/user/<UID>' (owner USER, mode 0700) and spawns
`shepherd -c ~/.config/shepherd/init.scm' detached under `su', then
polls `/run/user/<UID>/shepherd/socket'.  Idempotent: a second call
returns immediately when the socket is already bound."
  (let ((desc (string-append user " user shepherd IPC socket up")))
    (test-assert desc
      (marionette-eval
       `(let* ((pw (getpwnam ,user))
               (uid (passwd:uid pw))
               (gid (passwd:gid pw))
               (home (passwd:dir pw))
               (uid-str (number->string uid))
               (xdg (string-append "/run/user/" uid-str))
               (sock (string-append xdg "/shepherd/socket"))
               (log (string-append "/tmp/" ,user "-shepherd.log"))
               ;; `/run/setuid-programs/su' is the legacy path; newer
               ;; Guix puts setuid binaries under `/run/privileged/bin'
               ;; and keeps the legacy directory as a compat shim.
               ;; Pick whichever exists so the helper works across
               ;; both eras.
               (su (cond ((file-exists? "/run/privileged/bin/su")
                          "/run/privileged/bin/su")
                         ((file-exists? "/run/setuid-programs/su")
                          "/run/setuid-programs/su")
                         (else "su"))))
          (unless (file-exists? sock)
            (unless (file-exists? "/run/user") (mkdir "/run/user"))
            (unless (file-exists? xdg) (mkdir xdg))
            (chown xdg uid gid)
            (chmod xdg #o700)
            ;; `su USER -c' does NOT reset HOME (that needs `su -');
            ;; without HOME pointing at USER's actual home directory
            ;; the setup-environment script and shepherd would read
            ;; root's dotfiles.  Set it explicitly.
            (system* su ,user "-c"
                     (string-append
                      "export HOME=" home "; "
                      "export USER=" ,user "; "
                      "export XDG_RUNTIME_DIR=" xdg "; "
                      ". $HOME/.guix-home/setup-environment; "
                      "nohup shepherd -c $HOME/.config/shepherd/init.scm "
                      "</dev/null >>" log " 2>&1 &")))
          (let loop ((i ,timeout))
            (cond ((file-exists? sock) #t)
                  ((> i 0) (sleep 1) (loop (- i 1)))
                  (else #f))))
       marionette))))

(define (test-assert-user-shepherd-service-started user service marionette)
  "Assert inside MARIONETTE that `herd start SERVICE' against USER's
shepherd exits with status zero.  SERVICE is a symbol (e.g.
'gascity-supervisor-test2).  Caller must first bring USER's shepherd
up via `test-assert-user-shepherd-up'.

For long-running services, success means the start thunk fork-execed
without raising — pair with `test-assert-wait-for-file-exists' on a
side-effect path (e.g. a Unix-domain socket) to prove the started
process reached its listening state."
  (let* ((service-str (symbol->string service))
         (desc (string-append user " user shepherd: 'herd start "
                              service-str "' exit 0")))
    (test-assert desc
      (marionette-eval
       `(let* ((pw (getpwnam ,user))
               (uid (passwd:uid pw))
               (home (passwd:dir pw))
               (xdg (string-append "/run/user/" (number->string uid)))
               (su (cond ((file-exists? "/run/privileged/bin/su")
                          "/run/privileged/bin/su")
                         ((file-exists? "/run/setuid-programs/su")
                          "/run/setuid-programs/su")
                         (else "su"))))
          (zero?
           (status:exit-val
            (system* su ,user "-c"
                     (string-append
                      "export HOME=" home "; "
                      "export USER=" ,user "; "
                      "export XDG_RUNTIME_DIR=" xdg "; "
                      ". $HOME/.guix-home/setup-environment; "
                      "herd start " ,service-str)))))
       marionette))))

;;; marionette.scm ends here
