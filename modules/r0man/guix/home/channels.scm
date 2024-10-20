(define-module (r0man guix home channels)
  #:use-module (r0man guix channels)
  #:use-module (gnu home services guix)
  #:use-module (gnu home services)
  #:use-module (guix ci)
  #:export (channels home-channels-services))

(define channels
  (list asahi-channel
        guix-channel
        nonguix-channel
        r0man-guix-channel))

(define home-channels-services
  (list (service home-channels-service-type channels)))

channels
