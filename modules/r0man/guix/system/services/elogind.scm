(define-module (r0man guix system services elogind)
  #:use-module (gnu services)
  #:use-module (gnu services desktop)
  #:export (%elogind-service))

(define %elogind-service
  (service elogind-service-type))
