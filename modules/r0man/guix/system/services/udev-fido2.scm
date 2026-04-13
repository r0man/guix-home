(define-module (r0man guix system services udev-fido2)
  #:use-module (gnu packages security-token)
  #:use-module (gnu services base)
  #:export (%udev-fido2-service))

(define %udev-fido2-service
  (udev-rules-service 'fido2 libfido2 #:groups '("plugdev")))
