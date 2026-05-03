;;; Copyright © 2026 Roman Scherer <roman@burningswell.com>
;;;
;;; This file is part of the r0man Guix Home channel.

(define-module (r0man guix tests systems base)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services networking)
  #:use-module (gnu system vm)
  #:use-module (gnu tests)
  #:use-module (guix gexp)
  #:use-module (r0man guix tests)
  #:export (%test-r0man-base-os))

;;; Commentary:
;;;
;;; Marionette test that boots `%r0man-simple-os` (the minimal x86_64
;;; OS used by r0man Guix Home tests) and asserts the basic shepherd
;;; services that come with `%base-services` (file-system,
;;; user-processes, syslogd) actually start.  This is the regression
;;; gate for the test scaffolding itself — if this fails, the home
;;; tests are not trustworthy.
;;;
;;; Note: We do NOT use `m1-operating-system` from
;;; `(r0man guix system m1)` here.  Its asahi-linux kernel,
;;; m1n1-u-boot-grub bootloader, and pinned cryptroot UUID make it
;;; un-bootable under stock x86 QEMU.
;;;
;;; Code:

(define (run-r0man-base-os-test)
  "Run the base-OS marionette test in a VM."
  (define os
    (marionette-operating-system
     %r0man-simple-os
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
          (test-begin "r0man-base-os")

          (test-assert-service-running 'user-processes marionette)
          (test-assert-service-running 'syslogd marionette)
          (test-assert-user-exists "alice" marionette)

          (test-end))))
  (gexp->derivation "r0man-base-os-test" test))

(define %test-r0man-base-os
  (system-test
   (name "r0man-base-os")
   (description "Boot %r0man-simple-os and verify base shepherd
services (user-processes, syslogd) come up and the test-account user
`alice' exists.")
   (value (run-r0man-base-os-test))))

;;; base.scm ends here
