(define-module (r0man guix home audio)
  #:use-module (gnu home services desktop)
  #:use-module (gnu packages linux)
  #:use-module (gnu services base)
  #:use-module (gnu services)
  #:use-module (r0man guix home pipewire)
  #:export (home-audio-services))

(define home-audio-services
  (list (service home-dbus-service-type)
        (service home-pipewire-service-type)))
