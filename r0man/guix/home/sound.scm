(define-module (r0man guix home sound)
  #:use-module (asahi guix home services sound)
  #:use-module (gnu home services))

(define-public home-pipewire-services
  (list (service home-pipewire-service-type)))
