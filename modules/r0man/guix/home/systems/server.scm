(define-module (r0man guix home systems server)
  #:use-module (gnu home services)
  #:use-module (gnu home)
  #:use-module (gnu services)
  #:use-module (r0man guix home bash)
  #:use-module (r0man guix home btop)
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
  #:use-module (r0man guix home mbsync)
  #:use-module (r0man guix home msmtp)
  #:use-module (r0man guix home nix)
  #:use-module (r0man guix home packages)
  #:use-module (r0man guix home pm)
  #:use-module (r0man guix home shepherd)
  #:use-module (r0man guix home sound)
  #:use-module (r0man guix home ssh))

(define services
  (append home-bash-services
          home-btop-services
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
          home-mbsync-services
          home-msmtp-services
          home-nix-services
          home-pm-services
          home-shepherd-services
          home-ssh-services))

(define-public server-home-environment
  (home-environment
   (packages packages)
   (services services)))

server-home-environment
