(define-module (r0man guix system services auditd)
  #:use-module (gnu services)
  #:use-module (gnu services auditd)
  #:export (%auditd-service-type))

(define %auditd-service-type
  (service auditd-service-type))
