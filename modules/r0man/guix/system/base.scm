(define-module (r0man guix system base)
  #:use-module (gnu packages linux)
  #:use-module (gnu system nss)
  #:use-module (gnu)
  #:use-module (guix packages)
  #:use-module (r0man guix system keyboard)
  #:use-module (r0man guix system services)
  #:export (base-operating-system))

(define %packages
  (map specification->package
       '("cryptsetup-static"
         "e2fsprogs"
         "emacs"
         "lvm2-static"
         "net-tools"
         "network-manager")))

(define %supplementary-groups
  '("audio" "cgroup" "libvirt" "lp" "netdev" "plugdev" "video" "wheel"))

(define %users
  (list (user-account
         (name "roman")
         (comment "Roman")
         (group "users")
         (home-directory "/home/roman")
         (supplementary-groups %supplementary-groups))))

(define %services
  (modify-services (cons* %libvirt-service
                          %openssh-service
                          %base-services)
    (console-font-service-type config => (console-font-service-config config))
    (guix-service-type config => (guix-service-type-config config))))

(define-public base-operating-system
  (operating-system
    (host-name "base")
    (locale "en_US.utf8")
    (timezone "Europe/Berlin")
    (keyboard-layout %keyboard-layout)
    (bootloader (bootloader-configuration
                 (bootloader grub-efi-removable-bootloader)
                 (targets (list "/boot/efi"))
                 (keyboard-layout keyboard-layout)))
    (kernel-arguments
     (append '("modprobe.blacklist=radeon"
               ;; Prevent network interfaces from having really long
               ;; names. This can cause an issue with wpa_supplicant
               ;; when you try to connect to a wifi network.
               "net.ifnames=0"
               ;; "quiet"
               )
             %default-kernel-arguments))
    (file-systems (cons*
                   (file-system
                     (mount-point "/")
                     (device "none")
                     (type "tmpfs")
                     (check? #f))
                   %base-file-systems))
    (users (append %users %base-user-accounts))
    (packages (append %packages %base-packages))
    (services %services)))

base-operating-system
