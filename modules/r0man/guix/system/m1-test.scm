(define-module (r0man guix system m1-test)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system mapped-devices)
  #:use-module (gnu system uuid)
  #:use-module (r0man guix system m1))

(define %file-systems
  (cons* (file-system
           (device (uuid "86E8-4498" 'fat32)) ;; /dev/nvme0n1p12
           (mount-point "/boot/efi")
           (needed-for-boot? #t)
           (type "vfat"))
         (file-system
           (device (file-system-label "asahi-guix-root")) ;; /dev/nvme0n1p14
           (mount-point "/")
           (needed-for-boot? #t)
           (type "btrfs"))
         %base-file-systems))

(define %swap-devices
  (list (swap-space
         (target "/dev/mapper/bombaclaat-swap"))))

(define-public m1-test-operating-system
  (operating-system
    (inherit m1-operating-system)
    (host-name "m1-test")
    (file-systems %file-systems)
    (swap-devices %swap-devices)))

m1-test-operating-system
