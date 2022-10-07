(define-module (r0man home config)
  #:use-module (gnu home services)
  #:use-module (gnu home)
  #:use-module (gnu services)
  #:use-module (r0man home bash)
  #:use-module (r0man home channels)
  #:use-module (r0man home clojure)
  #:use-module (r0man home common-lisp)
  #:use-module (r0man home emacs)
  #:use-module (r0man home environment)
  #:use-module (r0man home mcron)
  #:use-module (r0man home packages)
  #:use-module (r0man home profiles)
  #:use-module (r0man home shepherd)
  #:use-module (r0man home stumpwm)
  #:use-module (r0man home xdg)
  #:use-module (r0man home x11))

(define services
  (append home-bash-services
          home-channels-services
          home-clojure-services
          home-common-lisp-services
          home-emacs-services
          home-environment-variables-services
          home-mcron-services
          home-profiles-services
          home-shepherd-services
          home-stumpwm-services
          home-xdg-services
          home-x11-services))

(home-environment
 (packages packages)
 (services services))
