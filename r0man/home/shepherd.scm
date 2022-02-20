(define-module (r0man home shepherd)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services)
  #:use-module (r0man home emacs)
  #:export (home-shepherd-services))

(define home-shepherd-services
  (list (service
         home-shepherd-service-type
         (home-shepherd-configuration
          (services (list emacs-shepherd-service))))))
