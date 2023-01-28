(define-module (r0man guix home config)
  #:use-module (gnu home services)
  #:use-module (gnu home)
  #:use-module (gnu services)
  #:use-module (r0man guix home bash)
  #:use-module (r0man guix home channels)
  #:use-module (r0man guix home clojure)
  #:use-module (r0man guix home common-lisp)
  #:use-module (r0man guix home emacs)
  #:use-module (r0man guix home environment)
  #:use-module (r0man guix home gpg)
  #:use-module (r0man guix home guile)
  #:use-module (r0man guix home mcron)
  #:use-module (r0man guix home packages)
  #:use-module (r0man guix home shepherd)
  #:use-module (r0man guix home stumpwm)
  #:use-module (r0man guix home sway)
  #:use-module (r0man guix home xdg)
  #:use-module (r0man guix home x11))

(define services
  (append home-bash-services
          home-channels-services
          home-clojure-services
          home-common-lisp-services
          home-emacs-services
          home-environment-variables-services
          home-gpg-services
          home-guile-services
          home-mcron-services
          home-shepherd-services
          home-stumpwm-services
          home-sway-services
          home-xdg-services
          home-x11-services))

(home-environment
 (packages packages)
 (services services))
