(define-module (r0man guix system services pcscd)
  #:use-module (gnu services)
  #:use-module (gnu services security-token)
  #:export (%pcscd-service))

(define %pcscd-service
  (service pcscd-service-type))
