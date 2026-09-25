;;; (r0man guix system m1-runner-vm)
;;;
;;; A VM-bootable variant of the m1 operating-system, to try the
;;; GitHub Actions runner stack it carries in a QEMU VM before
;;; reconfiguring the real system.  The bootloader, mapped devices,
;;; swap and file systems are replaced by what 'guix system vm'
;;; boots; SDDM is dropped; the runners register under the VM's own
;;; host name.
;;;
;;;   guix system vm -L modules modules/r0man/guix/system/m1-runner-vm.scm
;;;
;;; In the VM, as root: create /etc/github-runner-pat-bs (0600), then
;;;
;;;   herd start github-actions-runner-token
;;;   herd start github-actions-runner-ci-1
;;;   herd start github-actions-runner-ci-2

(define-module (r0man guix system m1-runner-vm)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader u-boot)
  #:use-module (gnu services)
  #:use-module (gnu services sddm)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (r0man guix services github-actions)
  #:use-module (r0man guix system m1)
  #:use-module (srfi srfi-1))

(define-public m1-runner-vm-operating-system
  (operating-system
    (inherit m1-operating-system)
    (host-name "m1-runner-vm")
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
     (modify-services
         (remove (lambda (service)
                   (memq (service-kind service)
                         (list sddm-service-type)))
                 (operating-system-user-services m1-operating-system))
       (github-actions-runner-service-type
        configs =>
        (map (lambda (config)
               (github-actions-runner-configuration
                (inherit config)
                (name #f)))
             configs))))))

m1-runner-vm-operating-system
