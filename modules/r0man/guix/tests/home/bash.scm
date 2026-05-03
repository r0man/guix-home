;;; Copyright © 2026 Roman Scherer <roman@burningswell.com>
;;;
;;; This file is part of the r0man Guix Home channel.

(define-module (r0man guix tests home bash)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu services)
  #:use-module (gnu services guix)
  #:use-module (gnu system vm)
  #:use-module (gnu tests)
  #:use-module (guix gexp)
  #:use-module (r0man guix tests)
  #:export (%test-home-bash))

;;; Commentary:
;;;
;;; Marionette test that boots a minimal x86_64 OS containing a
;;; `guix-home-service-type` for user `alice` whose home environment
;;; has `home-bash-service-type`, then asserts /home/alice/.bashrc
;;; exists post-activation.
;;;
;;; This is a marionette test (boots a VM) — NOT a derivation-only
;;; test, because home-environment derivations don't run activation;
;;; only `guix-home-service-type` inside a booted OS does.
;;;
;;; Code:

(define %home-bash-environment
  (home-environment
   (services (list (service home-bash-service-type)))))

(define %home-bash-os
  (r0man-simple-operating-system
   (service guix-home-service-type
            `(("alice" ,%home-bash-environment)))))

(define (run-home-bash-test)
  "Run the home-bash marionette test in a VM."
  (define os
    (marionette-operating-system
     %home-bash-os
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
          (test-begin "home-bash")

          (test-assert-home-bashrc-exists "alice" marionette)

          (test-end))))
  (gexp->derivation "r0man-home-bash-test" test))

(define %test-home-bash
  (system-test
   (name "r0man-home-bash")
   (description "Boot a minimal OS with guix-home-service-type wrapping
home-bash-service-type for alice and verify activation produces
/home/alice/.bashrc.")
   (value (run-home-bash-test))))

;;; bash.scm ends here
