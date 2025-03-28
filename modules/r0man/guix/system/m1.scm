(define-module (r0man guix system m1)
  #:use-module ((gnu services sound) #:prefix sound:)
  #:use-module (asahi guix initrd)
  #:use-module (asahi guix packages audio)
  #:use-module (asahi guix packages display-managers)
  #:use-module (asahi guix packages gl)
  #:use-module (asahi guix packages linux)
  #:use-module (asahi guix packages misc)
  #:use-module (asahi guix packages wm)
  #:use-module (asahi guix packages xorg)
  #:use-module (asahi guix services firmware)
  #:use-module (asahi guix services sound)
  #:use-module (asahi guix services udev)
  #:use-module (gnu  bootloader m1n1)
  #:use-module (gnu bootloader)
  #:use-module (gnu packages display-managers)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xorg)
  #:use-module (gnu services base)
  #:use-module (gnu services containers)
  #:use-module (gnu services guix)
  #:use-module (gnu services linux)
  #:use-module (gnu services networking)
  #:use-module (gnu services sddm)
  #:use-module (gnu services xorg)
  #:use-module (gnu services)
  #:use-module (gnu system accounts)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system mapped-devices)
  #:use-module (gnu system nss)
  #:use-module (gnu system uuid)
  #:use-module (gnu system)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix packages)
  #:use-module (r0man guix channels)
  #:use-module (r0man guix home systems m1)
  #:use-module (r0man guix packages display-managers)
  #:use-module (r0man guix system desktop)
  #:use-module (r0man guix system keyboard)
  #:use-module (r0man guix system services)
  #:use-module (r0man guix system xorg)
  #:use-module (srfi srfi-1))

(define %bootloader
  (bootloader-configuration
   (bootloader m1n1-u-boot-grub-bootloader)
   (targets (list "/boot/efi"))
   (keyboard-layout %keyboard-layout)))

(define %channels
  (list asahi-channel
        guix-channel
        nonguix-channel
        r0man-guix-channel))

(define %home-service
  (service guix-home-service-type `(("roman" ,m1-home-environment))))

(define %packages
  (cons* asahi-alsa-utils
         asahi-mesa-utils
         asahi-sway
         asahi-scripts
         network-manager
         (remove (lambda (package)
                   (equal? "network-manager" (package-name package)))
                 (map replace-mesa
                      (cons* hyprland
                             stumpwm
                             (operating-system-packages desktop-operating-system))))))

(define %mapped-devices
  (list (mapped-device
         (source (uuid "56b68fba-21cb-49b5-ac44-84dc382f3426"))
         (target "cryptroot")
         (type luks-device-mapping))
        (mapped-device
         (source "bombaclaat")
         (targets (list "bombaclaat-root"
                        "bombaclaat-swap"))
         (type lvm-device-mapping))))

(define %file-systems
  (cons* (file-system
           (mount-point "/")
           (device "/dev/mapper/bombaclaat-root")
           (type "ext4")
           (needed-for-boot? #t)
           (dependencies %mapped-devices))
         (file-system
           (mount-point "/boot/efi")
           (device (uuid "9FBE-130E" 'fat32))
           (type "vfat"))
         %base-file-systems))

(define %xorg-configuration
  (xorg-configuration
   (keyboard-layout %keyboard-layout)
   (modules (map replace-mesa
                 (list xf86-video-fbdev
                       xf86-input-libinput
                       xf86-input-evdev
                       xf86-input-mouse)))
   (extra-config (list %xorg-libinput-config
                       %xorg-modeset-config))
   (server asahi-xorg-server)))

(define %sddm-service
  (service sddm-service-type
           (sddm-configuration
            (auto-login-user "roman")
            (sddm (replace-mesa sddm-qt5))
            (theme "guix-sugar-light")
            (themes-directory (file-append guix-sugar-light-sddm-theme "/share/sddm/themes"))
            (xorg-configuration %xorg-configuration))))

(define %rootless-podman-service
  (service rootless-podman-service-type
           (rootless-podman-configuration
            (subgids
             (list (subid-range (name "roman"))))
            (subuids
             (list (subid-range (name "roman")))))))

(define %asahi-kernel-module-config
  (simple-service 'asahi-config etc-service-type
                  (list `("modprobe.d/asahi.conf"
                          ,(plain-file "asahi.conf" "options asahi debug_flags=1")))))

(define %services
  (modify-services (cons* (service alsa-service-type)
                          (service asahi-firmware-service-type)
                          (service kernel-module-loader-service-type '("asahi" "appledrm"))
                          (service sound:speakersafetyd-service-type)
                          (service iptables-service-type)
                          %asahi-kernel-module-config
                          %home-service
                          %qemu-service-aarch64
                          %rootless-podman-service
                          %sddm-service
                          %udev-backlight-service
                          %udev-kbd-backlight-service
                          (operating-system-user-services desktop-operating-system))
    (delete sound:alsa-service-type)
    (delete sound:pulseaudio-service-type)
    (delete slim-service-type)
    (guix-service-type
     config =>
     (guix-configuration
      (inherit config)
      (authorized-keys
       (cons* (local-file "./keys/asahi-guix.pub")
              (local-file "./keys/nonguix.pub")
              (local-file "./keys/precision.pub")
              %default-authorized-guix-keys))
      (build-machines
       (list #~(build-machine
                (name "www.asahi-guix.org")
                (systems (list "aarch64-linux"))
                (user "root")
                (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFZPlAeMk4hiHKHDCoDd1bT/ddMaZZR0iMWHE/mCGDFX"))
             #~(build-machine
                (name "precision.local")
                (systems (list "x86_64-linux"))
                (user "root")
                (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPgG2A1s40fYCkbqYW4K3Hs+Ap/iFDysLiw/8OXZLFRZ"))))
      (channels %channels)
      ;; TODO: Is this causing a pull on guix system commands?
      ;; (guix (guix-for-channels %channels))
      (substitute-urls
       (cons* "https://substitutes.asahi-guix.org"
              "https://substitutes.nonguix.org"
              %default-substitute-urls))))))

(define %swap-devices
  (list (swap-space
          (target "/dev/mapper/bombaclaat-swap")
          (dependencies %mapped-devices))))

(define-public m1-operating-system
  (operating-system
    (inherit desktop-operating-system)
    (host-name "m1")
    (bootloader %bootloader)
    (kernel asahi-linux)
    (initrd-modules asahi-initrd-modules)
    (mapped-devices %mapped-devices)
    (file-systems %file-systems)
    (packages %packages)
    (services %services)
    (swap-devices %swap-devices)))

m1-operating-system
