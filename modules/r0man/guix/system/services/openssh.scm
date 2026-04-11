(define-module (r0man guix system services openssh)
  #:use-module (gnu packages ssh)
  #:use-module (gnu services)
  #:use-module (gnu services ssh)
  #:export (%openssh-service))

(define %openssh-service
  (service openssh-service-type
           (openssh-configuration
            (openssh openssh)
            (permit-root-login 'prohibit-password)
            (port-number 22)
            (x11-forwarding? #t))))
