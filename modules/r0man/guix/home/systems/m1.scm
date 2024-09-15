(define-module (r0man guix home systems m1)
  #:use-module (gnu home services)
  #:use-module (gnu home)
  #:use-module (gnu services)
  #:use-module (r0man guix home bash)
  #:use-module (r0man guix home channels)
  #:use-module (r0man guix home clojure)
  #:use-module (r0man guix home common-lisp)
  #:use-module (r0man guix home desktop)
  #:use-module (r0man guix home emacs)
  #:use-module (r0man guix home environment)
  #:use-module (r0man guix home fzf)
  #:use-module (r0man guix home git)
  #:use-module (r0man guix home gpg)
  #:use-module (r0man guix home guile)
  #:use-module (r0man guix home i3status)
  #:use-module (r0man guix home kitty)
  #:use-module (r0man guix home librewolf)
  #:use-module (r0man guix home mbsync)
  #:use-module (r0man guix home nix)
  #:use-module (r0man guix home packages)
  #:use-module (r0man guix home shepherd)
  #:use-module (r0man guix home sound)
  #:use-module (r0man guix home stumpwm)
  #:use-module (r0man guix home sway)
  #:use-module (r0man guix home wofi)
  #:use-module (r0man guix home xdg)
  #:use-module (r0man guix home x11))

(define services
  (append home-bash-services
          home-channels-services
          home-clojure-services
          home-common-lisp-services
          home-dbus-services
          home-emacs-services
          home-environment-variables-services
          home-fzf-services
          home-git-services
          home-gpg-services
          home-guile-services
          home-i3status-services
          home-kitty-services
          home-librewolf-services
          home-mbsync-services
          home-nix-services
          home-pipewire-services-m1
          home-shepherd-services
          home-stumpwm-services
          home-sway-services
          home-wofi-services
          home-xdg-services
          home-x11-services))

(define-public m1-home-environment
  (home-environment
   (packages packages)
   (services services)))

m1-home-environment
