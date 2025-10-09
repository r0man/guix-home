(define-module (r0man guix home systems precision)
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
          home-btop-services
          home-channels-services
          home-clojure-services
          home-common-lisp-services
          home-dbus-services
          (list (service home-eca-service-type)
                (service home-git-service-type))
          home-emacs-services
          home-environment-variables-services
          home-fzf-services
          home-gpg-gtk-services
          home-guile-services
          home-hyprland-services
          home-i3status-services
          home-kitty-services
          home-librewolf-services
          home-mbsync-services
          home-msmtp-services
          home-nix-services
          home-pipewire-services-precision
          home-pm-services
          home-rofi-services
          home-shepherd-services
          home-ssh-services
          home-stumpwm-services
          home-sway-services
          home-waybar-services
          home-wofi-services
          home-xdg-services
          home-x11-services))

(define-public precision-home-environment
  (home-environment
   (packages packages)
   (services services)))

precision-home-environment
