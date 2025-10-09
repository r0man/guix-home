(define-module (r0man guix home systems m1)
  #:use-module (gnu home services)
  #:use-module (gnu home)
  #:use-module (gnu services)
  #:use-module (r0man guix home bash)
  #:use-module (r0man guix home btop)
  #:use-module (r0man guix home channels)
  #:use-module (r0man guix home clojure)
  #:use-module (r0man guix home common-lisp)
  #:use-module (r0man guix home desktop)
  #:use-module (r0man guix home eca)
  #:use-module (r0man guix home emacs)
  #:use-module (r0man guix home environment)
  #:use-module (r0man guix home fzf)
  #:use-module (r0man guix home git)
  #:use-module (r0man guix home gpg)
  #:use-module (r0man guix home guile)
  #:use-module (r0man guix home hyprland)
  #:use-module (r0man guix home i3status)
  #:use-module (r0man guix home kitty)
  #:use-module (r0man guix home librewolf)
  #:use-module (r0man guix home mbsync)
  #:use-module (r0man guix home msmtp)
  #:use-module (r0man guix home nix)
  #:use-module (r0man guix home packages)
  #:use-module (r0man guix home pm)
  #:use-module (r0man guix home rofi)
  #:use-module (r0man guix home shepherd)
  #:use-module (r0man guix home sound)
  #:use-module (r0man guix home ssh)
  #:use-module (r0man guix home stumpwm)
  #:use-module (r0man guix home sway)
  #:use-module (r0man guix home waybar)
  #:use-module (r0man guix home wofi)
  #:use-module (r0man guix home xdg)
  #:use-module (r0man guix home x11))

(define services
  (append home-bash-services
          (list (service home-btop-service-type))
          home-channels-services
          (list (service home-clojure-service-type)
                (service home-common-lisp-service-type))
          home-dbus-services
          (list (service home-eca-service-type)
                (service home-fzf-service-type)
                (service home-git-service-type)
                (service home-guile-service-type))
          home-emacs-services
          home-environment-variables-services
          home-gpg-gtk-services
          home-hyprland-services
          (list (service home-i3status-service-type)
                (service home-kitty-service-type)
                (service home-librewolf-service-type))
          home-mbsync-services
          home-msmtp-services
          (list (service home-nix-service-type))
          home-pipewire-services-m1
          home-pm-services
          (list (service home-rofi-service-type))
          home-shepherd-services
          home-ssh-services
          (list (service home-stumpwm-service-type))
          home-sway-services
          (list (service home-waybar-service-type)
                (service home-wofi-service-type))
          home-xdg-services
          home-x11-services))

(define-public m1-home-environment
  (home-environment
   (packages packages)
   (services services)))

m1-home-environment
