(define-module (r0man guix home pm)
  #:use-module (gnu home services pm)
  #:use-module (gnu home services)
  #:export (home-pm-services))

(define home-pm-services
  (list (service home-batsignal-service-type)))
