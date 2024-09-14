(define-module (r0man guix home sound)
  #:use-module ((asahi guix home services sound) #:prefix asahi:)
  #:use-module (gnu home services sound)
  #:use-module (gnu home services))

(define-public home-pipewire-services-m1
  (list (service asahi:home-pipewire-service-type)))

(define-public home-pipewire-services-precision
  (list (service home-pipewire-service-type)))
