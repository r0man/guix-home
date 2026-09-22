;;; (r0man guix system m1-runner-vm)
;;;
;;; A VM-bootable variant of the m1 operating-system that adds the
;;; plain GitHub Actions runner stack (docker, registration token
;;; minting, runner service) for the burningswell project.  Used to
;;; try the runner stack in a QEMU VM on the laptop before configuring
;;; it into the real system.
;;;
;;; Overrides vs. m1-operating-system (everything else is inherited):
;;; the m1n1 bootloader is replaced with grub-efi (QEMU boots UEFI),
;;; the LUKS/LVM mapped devices and swap are dropped (no such hardware
;;; in a VM), and file systems use the Guix_image label that 'guix
;;; system vm' generates.  SDDM is deleted (headless).
;;;
;;; Boot with:
;;;
;;;   guix system vm -L modules modules/r0man/guix/system/m1-runner-vm.scm
;;;
;;; and copy the printed run-vm.sh store path; run it from a terminal
;;; (KVM is used automatically).  Log in as root on the serial console
;;; (empty password), create /etc/github-runner-pat-bs with the
;;; fine-grained PAT, then:
;;;
;;;   herd start mint-registration-token
;;;   herd start github-actions-runner

(define-module (r0man guix system m1-runner-vm)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader u-boot)
  #:use-module (gnu services)
  #:use-module (gnu services docker)
  #:use-module (gnu services sddm)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (guix gexp)
  #:use-module (r0man guix services github-actions)
  #:use-module (r0man guix services github-actions-vm)
  #:use-module (r0man guix system m1)
  #:use-module (srfi srfi-1))

(define-public %runner-vm-runner-service
  ;; The plain (non-VM) GitHub Actions runner for the burningswell
  ;; project.  The one-shot mint service below provisions a fresh
  ;; registration token before the runner starts; the start script
  ;; reads the token from an absolute path at start time.
  (service github-actions-runner-service-type
           (github-actions-runner-configuration
            (url "https://github.com/burningswell/burningswell-cl")
            (token "/var/lib/github-actions-runner/token")
            (name "m1-vm-bs-runner")
            (labels '("docker" "burningswell"))
            (replace? #t)
            (supplementary-groups '("docker"))
            (requirements '(mint-registration-token dockerd)))))

(define-public %runner-vm-mint-service
  (simple-service 'github-actions-runner-mint
                  shepherd-root-service-type
                  (list (shepherd-service
                         (documentation
                          "Mint a GitHub Actions runner registration token.")
                         (provision '(mint-registration-token))
                         (one-shot? #t)
                         (start #~(lambda ()
                                    (let ((rc (system*
                                               #$(github-actions-runner-vm-mint-program)
                                               "/etc/github-runner-pat-bs"
                                               "https://github.com/burningswell/burningswell-cl"
                                               "/var/lib/github-actions-runner/token")))
                                      (if (zero? rc)
                                          ;; The runner start script reads
                                          ;; the token as the runner user.
                                          (begin
                                            (chmod
                                             "/var/lib/github-actions-runner/token"
                                             #o644)
                                            #t)
                                          #f))))
                         (stop #~(const #f))))))

(define-public m1-runner-vm-operating-system
  (operating-system
    (inherit m1-operating-system)
    (host-name "m1-runner-vm")
    ;; 'guix system vm' boots the kernel directly (-kernel), so the
    ;; bootloader is a placeholder that builds nothing on any
    ;; architecture (the m1n1/EFI bootloader of the real hardware
    ;; cannot boot there; BIOS grub does not build for aarch64).
    (bootloader (bootloader-configuration
                 (bootloader u-boot-bootloader)
                 (targets '("/dev/sdX"))))
    (initrd-modules (append '("virtio_net" "virtio_blk")
                            (operating-system-initrd-modules
                             m1-operating-system)))
    (mapped-devices '())
    (swap-devices '())
    (file-systems
     (cons* (file-system
              (device (file-system-label "Guix_image"))
              (mount-point "/")
              (needed-for-boot? #t)
              (type "ext4"))
            %base-file-systems))
    (services
     (cons*
      ;; In a 'guix system vm' the root file system is an overlay, on
      ;; which dockerd's overlay2 driver cannot create its upper
      ;; directories, and the kernel lacks the device-mapper thin-pool
      ;; target; fall back to the always-working vfs storage driver.
      (service docker-service-type
               (docker-configuration
                (config-file
                 (plain-file "dockerd-vm.json"
                             "{\"storage-driver\": \"vfs\"}"))))
      %runner-vm-mint-service
      %runner-vm-runner-service
      ;; Headless: drop SDDM and the X stack's display managers.
      (remove (lambda (service)
                (memq (service-kind service)
                      (list sddm-service-type)))
              (operating-system-user-services m1-operating-system))))))

m1-runner-vm-operating-system