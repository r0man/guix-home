(define-module (r0man guix system services docker)
  #:use-module (gnu services)
  #:use-module (gnu services docker)
  #:export (%docker-service))

(define %docker-service
  (service docker-service-type))
