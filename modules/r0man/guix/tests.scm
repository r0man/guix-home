;;; Copyright © 2026 Roman Scherer <roman@burningswell.com>
;;;
;;; This file is part of the r0man Guix Home channel.

(define-module (r0man guix tests)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system shadow)
  #:export (%r0man-simple-os
            r0man-simple-operating-system))

;;; Commentary:
;;;
;;; Minimal x86_64-friendly operating system for r0man Guix Home
;;; system tests.  Mirrors `(asahi guix tests)` in spirit but does not
;;; pin to Apple Silicon: stays on a generic GRUB bootloader and the
;;; QEMU x86_64 default machine so marionette tests can boot without a
;;; cross-compile or `-M virt`.  Do NOT swap in `m1-operating-system`
;;; from `(r0man guix system m1)` here — that pulls in `asahi-linux`,
;;; `m1n1-u-boot-grub-bootloader`, and a cryptroot UUID which together
;;; make the OS unbootable under stock x86 QEMU.
;;;
;;; Code:

(define %r0man-simple-os
  (operating-system
    (host-name "r0man-test")
    (timezone "Europe/Berlin")
    (locale "en_US.utf8")

    ;; Bootloader is a placeholder — Guix's marionette path uses
    ;; `virtual-machine` which boots the kernel directly via QEMU
    ;; -kernel, so this is never installed.
    (bootloader (bootloader-configuration
                 (bootloader grub-bootloader)
                 (targets '("/dev/sdX"))))
    (file-systems (cons (file-system
                          (device (file-system-label "my-root"))
                          (mount-point "/")
                          (type "ext4"))
                        %base-file-systems))
    (firmware '())

    (users (cons (user-account
                  (name "alice")
                  (comment "Test user")
                  (group "users")
                  (supplementary-groups '("wheel" "audio" "video")))
                 %base-user-accounts))))

(define-syntax-rule (r0man-simple-operating-system user-services ...)
  "Return an operating system that includes USER-SERVICES in addition
to %BASE-SERVICES, suitable for r0man Guix Home system tests."
  (operating-system
    (inherit %r0man-simple-os)
    (services (cons* user-services ... %base-services))))

;;; tests.scm ends here
