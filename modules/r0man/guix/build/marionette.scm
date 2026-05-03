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
            test-assert-home-bashrc-exists))

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

;;; marionette.scm ends here
