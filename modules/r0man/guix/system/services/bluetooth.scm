(define-module (r0man guix system services bluetooth)
  #:use-module (gnu services)
  #:use-module (gnu services desktop)
  #:export (%bluetooth-service))

(define %bluetooth-service
  (service bluetooth-service-type))
