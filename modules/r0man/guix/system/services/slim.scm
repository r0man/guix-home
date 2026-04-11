(define-module (r0man guix system services slim)
  #:use-module (gnu services)
  #:use-module (gnu services xorg)
  #:use-module (r0man guix system services keyboard)
  #:use-module (r0man guix system services xorg)
  #:export (%slim-service))

(define %slim-service
  (service slim-service-type
           (slim-configuration
            (xorg-configuration
             (xorg-configuration
              (keyboard-layout %keyboard-layout)
              (extra-config (list %xorg-libinput-config)))))))
