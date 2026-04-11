(define-module (r0man guix system services screen-locker)
  #:use-module (gnu packages suckless)
  #:use-module (gnu services)
  #:use-module (gnu services xorg)
  #:use-module (guix gexp)
  #:export (%screen-locker-service))

(define %screen-locker-service
  (service screen-locker-service-type
           (screen-locker-configuration
            (name "slock")
            (program (file-append slock "/bin/slock")))))
