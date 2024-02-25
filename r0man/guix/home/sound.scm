(define-module (r0man guix home sound)
  #:use-module ((asahi guix home services sound) #:prefix sound:))

(define-public home-pipewire-services
  (list sound:asahi-home-pipewire-service))
