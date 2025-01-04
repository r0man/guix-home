(define-module (r0man guix system vm)
  #:use-module ((gnu services sound) #:prefix sound:)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu bootloader)
  #:use-module (gnu packages display-managers)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xorg)
  #:use-module (gnu services base)
  #:use-module (gnu services guix)
  #:use-module (gnu services linux)
  #:use-module (gnu services sddm)
  #:use-module (gnu services spice)
  #:use-module (gnu services xorg)
  #:use-module (gnu services)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system mapped-devices)
  #:use-module (gnu system nss)
  #:use-module (gnu system uuid)
  #:use-module (gnu system)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix packages)
  #:use-module (r0man guix channels)
  #:use-module (r0man guix home systems precision)
  #:use-module (r0man guix packages display-managers)
  #:use-module (r0man guix system desktop)
  #:use-module (r0man guix system keyboard)
  #:use-module (r0man guix system services)
  #:use-module (r0man guix system xorg)
  #:use-module (srfi srfi-1))

(define %bootloader
  (bootloader-configuration
   (bootloader grub-bootloader)
   (targets '("/dev/vda"))
   (terminal-outputs '(console))))

(define %channels
  (list asahi-channel
        guix-channel
        nonguix-channel
        r0man-guix-channel))

(define %home-service
  (service guix-home-service-type `(("roman" ,precision-home-environment))))

(define %packages
  (cons* network-manager
         (remove (lambda (package)
                   (equal? "network-manager" (package-name package)))
                 (cons* hyprland
                        stumpwm
                        (operating-system-packages desktop-operating-system)))))

(define %file-systems
  (cons (file-system
          (mount-point "/")
          (device "/dev/vda1")
          (type "ext4"))
        %base-file-systems))

(define %sddm-service
  (service sddm-service-type
           (sddm-configuration
            (auto-login-user "roman")
            (theme "guix-sugar-light")
            (themes-directory (file-append guix-sugar-light-sddm-theme "/share/sddm/themes")))))

(define %services
  (modify-services (cons* %home-service
                          %sddm-service
                          (service spice-vdagent-service-type)
                          (operating-system-user-services desktop-operating-system))
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
      (channels %channels)
      ;; TODO: Is this causing a pull on guix system commands?
      ;; (guix (guix-for-channels %channels))
      (substitute-urls
       (cons* "https://substitutes.asahi-guix.org"
              "https://substitutes.nonguix.org"
              %default-substitute-urls))))))

(define-public vm-operating-system
  (operating-system
    (inherit desktop-operating-system)
    (host-name "precision")
    (bootloader %bootloader)
    (file-systems %file-systems)
    (packages %packages)
    (services %services)))

vm-operating-system
