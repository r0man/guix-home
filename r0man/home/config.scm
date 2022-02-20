(define-module (r0man home config)
  #:use-module (gnu home services)
  #:use-module (gnu home)
  #:use-module (gnu services)
  #:use-module (r0man home bash)
  #:use-module (r0man home emacs)
  #:use-module (r0man home environment)
  #:use-module (r0man home mcron)
  #:use-module (r0man home packages)
  #:use-module (r0man home shepherd)
  #:use-module (r0man home stumpwm))

(define services
  (append
   home-bash-services
   home-emacs-services
   home-environment-variables-services
   home-mcron-services
   home-shepherd-services
   home-stumpwm-services))

(home-environment
 (packages packages)
 (services services))
