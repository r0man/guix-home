(define-module (r0man home config)
  #:use-module (gnu home services)
  #:use-module (gnu home)
  #:use-module (gnu services)
  #:use-module (r0man home bash)
  #:use-module (r0man home environment)
  #:use-module (r0man home mcron)
  #:use-module (r0man home packages)
  #:use-module (r0man home shepherd))

(define services
  (list home-environment-variables-service
        home-bash-service
        home-mcron-service
        home-shepherd-service))

(home-environment
 (packages packages)
 (services services))
