(define-module (r0man guix system services postgresql)
  #:use-module (gnu packages databases)
  #:use-module (gnu services)
  #:use-module (gnu services databases)
  #:export (%postgresql-service))

(define %postgresql-service
  (service postgresql-service-type
           (postgresql-configuration
            (postgresql postgresql-15))))
