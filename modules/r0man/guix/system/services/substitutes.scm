(define-module (r0man guix system services substitutes)
  #:use-module (gnu services base)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:export (guix-moe-substitutes-service-type
            nonguix-substitutes-service-type))

(define nonguix-substitutes-service-type
  (service-type
   (name 'nonguix-substitutes)
   (extensions
    (list (service-extension guix-service-type
                             (const (guix-extension
                                     (authorized-keys
                                      (list (plain-file
                                             "nonguix.pub"
                                             "(public-key (ecc (curve Ed25519) (q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)))")))
                                     (substitute-urls
                                      '("https://substitutes.nonguix.org")))))))
   (default-value #f)
   (description
    "Configure the Nonguix substitute server, adding the signing key
and substitute URL to the guix-daemon configuration.")))

(define guix-moe-substitutes-service-type
  (service-type
   (name 'guix-moe-substitutes)
   (extensions
    (list (service-extension guix-service-type
                             (const (guix-extension
                                     (authorized-keys
                                      (list (plain-file
                                             "guix-moe-old.pub"
                                             "(public-key (ecc (curve Ed25519) (q #374EC58F5F2EC0412431723AF2D527AD626B049D657B5633AAAEBC694F3E33F9#)))")
                                            (plain-file
                                             "guix-moe-new.pub"
                                             "(public-key (ecc (curve Ed25519) (q #552F670D5005D7EB6ACF05284A1066E52156B51D75DE3EBD3030CD046675D543#)))")))
                                     (substitute-urls
                                      '("https://cache-cdn.guix.moe")))))))
   (default-value #f)
   (description
    "Configure the guix.moe substitute server, adding the signing keys
and substitute URL to the guix-daemon configuration.")))
