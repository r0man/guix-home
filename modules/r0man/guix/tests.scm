;;; Copyright © 2026 Roman Scherer <roman@burningswell.com>
;;;
;;; This file is part of the r0man Guix Home channel.

(define-module (r0man guix tests)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader u-boot)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system shadow)
  #:export (%r0man-marionette-qemu-args
            %r0man-simple-os
            r0man-simple-operating-system))

;;; Commentary:
;;;
;;; Minimal operating system for r0man Guix Home system tests.  Mirrors
;;; `(asahi guix tests)`: uses the package-less `u-boot-bootloader`
;;; placeholder, sets `console=ttyAMA0` so the aarch64 `-M virt` VM
;;; produces output, and exports `%r0man-marionette-qemu-args` with
;;; `-M virt` for the test files to thread through `make-marionette`.
;;; QEMU's default machine is bypassed by `marionette-operating-system`
;;; / `virtual-machine` direct-kernel boot, so no bootloader is ever
;;; installed.  Do NOT swap in `m1-operating-system` from
;;; `(r0man guix system m1)` here — its asahi-linux kernel and pinned
;;; cryptroot UUID make it un-bootable under stock QEMU virt.
;;;
;;; Code:

(define %r0man-simple-os
  (operating-system
    (host-name "r0man-test")
    (timezone "Europe/Berlin")
    (locale "en_US.UTF-8")

    ;; Bootloader is a placeholder — `virtual-machine` boots the kernel
    ;; directly via QEMU `-kernel`, so this is never installed.
    ;; `u-boot-bootloader` has no package and a no-op disk-image
    ;; installer, which keeps the OS derivation buildable on aarch64
    ;; (grub-pc / grub-efi need a buildable bootloader package or a GPT
    ;; image, neither of which `raw-with-offset-disk-image` provides).
    (bootloader (bootloader-configuration
                 (bootloader u-boot-bootloader)
                 (targets '("/dev/sdX"))))
    (file-systems (cons (file-system
                          (device (file-system-label "my-root"))
                          (mount-point "/")
                          (type "ext4"))
                        %base-file-systems))
    (kernel-arguments (cons "console=ttyAMA0"
                            (delete "quiet" %default-kernel-arguments)))
    (firmware '())

    (users (cons (user-account
                  (name "alice")
                  (comment "Test user")
                  (group "users")
                  (supplementary-groups '("wheel" "audio" "video")))
                 %base-user-accounts))))

(define %r0man-marionette-qemu-args
  ;; Required on aarch64 hosts (default qemu-system-aarch64 has no
  ;; default machine).  Harmless or unused on x86_64.
  '("-M" "virt"))

(define-syntax-rule (r0man-simple-operating-system user-services ...)
  "Return an operating system that includes USER-SERVICES in addition
to %BASE-SERVICES, suitable for r0man Guix Home system tests."
  (operating-system
    (inherit %r0man-simple-os)
    (services (cons* user-services ... %base-services))))

;;; tests.scm ends here
