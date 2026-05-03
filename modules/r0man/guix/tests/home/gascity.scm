;;; Copyright © 2026 Roman Scherer <roman@burningswell.com>
;;;
;;; This file is part of the r0man Guix Home channel.

(define-module (r0man guix tests home gascity)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu services)
  #:use-module (gnu services guix)
  #:use-module (gnu system vm)
  #:use-module (gnu tests)
  #:use-module (guix gexp)
  #:use-module (r0man guix home services gascity)
  #:use-module (r0man guix tests)
  #:export (%test-home-gascity))

;;; Commentary:
;;;
;;; Marionette test that boots a minimal x86_64 OS containing a
;;; `guix-home-service-type' for user `alice' whose home environment
;;; pairs `home-bash-service-type' (activation canary) with
;;; `home-gascity-service-type'.
;;;
;;; The gascity configuration sets `(packages '())' so the home-env
;;; build does not drag in dolt + beads-next via the default
;;; package list; the shepherd services still file-append gascity-next
;;; and git, which is sufficient to exercise the activation paths
;;; under test:
;;;
;;;   * .bashrc — generic home-environment activation canary.
;;;   * ~/.dolt/config_global.json seeded with the default identity
;;;     ("Gas City" / "gascity@localhost") — guards the
;;;     home-activation gexp at gascity.scm:299-324.  The
;;;     ~/.gitconfig short-circuit branch does not fire because no
;;;     home-git-service-type is in this home.
;;;   * setup-environment exports GC_HOME — guards commit a559c3e.
;;;   * Both gascity-supervisor and gascity-init are referenced by
;;;     ~/.config/shepherd/init.scm — locks down that the
;;;     home-shepherd-service-type extension wired both services in
;;;     (commit cbcbfcd's gascity-init -> gascity-supervisor pair).
;;;
;;; Code:

(define %home-gascity-environment
  (home-environment
   (services (list (service home-bash-service-type)
                   (service home-gascity-service-type
                            (home-gascity-configuration
                             (packages '())))))))

(define %home-gascity-os
  (r0man-simple-operating-system
   (service guix-home-service-type
            `(("alice" ,%home-gascity-environment)))))

(define (run-home-gascity-test)
  "Run the home-gascity marionette test in a VM."
  (define os
    (marionette-operating-system
     %home-gascity-os
     #:imported-modules '((gnu services herd))))

  (define vm
    (virtual-machine
     (operating-system os)
     (memory-size 1024)))

  (define test
    (with-imported-modules '((gnu build marionette)
                             (asahi guix build marionette)
                             (r0man guix build marionette))
      #~(begin
          (use-modules (srfi srfi-64)
                       (gnu build marionette)
                       (r0man guix build marionette))

          (define marionette
            (make-marionette (cons* #$vm '#$%r0man-marionette-qemu-args)))

          (test-runner-current (system-test-runner #$output))
          (test-begin "home-gascity")

          (test-assert-home-bashrc-exists "alice" marionette)

          (test-assert-file-exists
           "alice ~/.dolt/config_global.json seeded"
           "/home/alice/.dolt/config_global.json"
           marionette)

          (test-assert-file-contains
           "alice dolt config has default user.name"
           "/home/alice/.dolt/config_global.json"
           "Gas City"
           marionette)

          (test-assert-file-contains
           "alice dolt config has default user.email"
           "/home/alice/.dolt/config_global.json"
           "gascity@localhost"
           marionette)

          (test-assert-file-contains
           "alice setup-environment exports GC_HOME"
           "/home/alice/.guix-home/setup-environment"
           "GC_HOME="
           marionette)

          (test-assert-file-contains
           "alice's shepherd init.scm references gascity-supervisor"
           "/home/alice/.guix-home/files/.config/shepherd/init.scm"
           "gascity-supervisor"
           marionette)

          (test-assert-file-contains
           "alice's shepherd init.scm references gascity-init"
           "/home/alice/.guix-home/files/.config/shepherd/init.scm"
           "gascity-init"
           marionette)

          (test-end))))
  (gexp->derivation "r0man-home-gascity-test" test))

(define %test-home-gascity
  (system-test
   (name "r0man-home-gascity")
   (description "Boot a minimal OS with guix-home-service-type wrapping
home-bash-service-type and home-gascity-service-type for alice; verify
activation produces .bashrc, seeds ~/.dolt/config_global.json with the
default identity, exports GC_HOME via setup-environment, and
materialises the gascity-supervisor shepherd service.")
   (value (run-home-gascity-test))))

;;; gascity.scm ends here
