(define-module (r0man guix system services containerd)
  #:use-module (gnu services)
  #:use-module (gnu services docker)
  #:export (%containerd-service))

(define %containerd-service
  (service containerd-service-type))
