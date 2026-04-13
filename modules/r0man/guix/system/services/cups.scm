(define-module (r0man guix system services cups)
  #:use-module (gnu packages cups)
  #:use-module (gnu services)
  #:use-module (gnu services cups)
  #:export (%cups-service))

(define %cups-service
  (service cups-service-type
           (cups-configuration
            (web-interface? #t)
            (extensions
             (list cups-filters)))))
