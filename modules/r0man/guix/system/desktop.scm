(define-module (r0man guix system desktop)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages suckless)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (gnu services avahi)
  #:use-module (gnu services base)
  #:use-module (gnu services dbus)
  #:use-module (gnu services desktop)
  #:use-module (gnu services networking)
  #:use-module (gnu services sddm)
  #:use-module (gnu services sound)
  #:use-module (gnu services xorg)
  #:use-module (gnu services)
  #:use-module (gnu system nss)
  #:use-module (gnu system)
  #:use-module (guix utils)
  #:use-module (r0man guix packages lisp)
  #:use-module (r0man guix system base)
  #:use-module (r0man guix system services)
  #:use-module (r0man guix system services substitutes)
  #:re-export (%roman)
  #:use-module (srfi srfi-1))

(define %packages
  (list
   blueman
   xf86-input-libinput))

(define (network-manager-applet? service)
  (eq? 'network-manager-applet
       (service-type-name (service-kind service))))

(define %services
  (remove (lambda (service)
            (network-manager-applet? service))
          (modify-services (cons* (service guix-moe-substitutes-service-type)
                                    (service nonguix-substitutes-service-type)
                                    %auditd-service-type
                                    %bluetooth-service
                                    %containerd-service
                                    %cups-service
                                    %libvirt-service
                                    %nix-service
                                    %openssh-service
                                    %pcscd-service
                                    %slim-service
                                    %udev-fido2-service
                                    %desktop-services)
              (delete gdm-service-type)
              (console-font-service-type config => (console-font-service-config config)))))

(define-public desktop-operating-system
  (operating-system
    (inherit base-operating-system)
    (name-service-switch %mdns-host-lookup-nss)
    (packages (append %packages (operating-system-packages base-operating-system)))
    (services %services)))

desktop-operating-system
