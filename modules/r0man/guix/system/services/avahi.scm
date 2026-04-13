(define-module (r0man guix system services avahi)
  #:use-module (gnu services)
  #:use-module (gnu services avahi)
  #:export (%avahi-service))

(define %avahi-service
  (service avahi-service-type))
