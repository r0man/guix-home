(define-module (r0man guix home shepherd)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services)
  #:use-module (r0man guix home emacs)
  #:export (home-shepherd-services))

(define home-shepherd-services
  (list))
