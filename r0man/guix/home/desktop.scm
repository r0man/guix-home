(define-module (r0man guix home desktop)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services))

(define-public home-dbus-services
  (list (service home-dbus-service-type)))
