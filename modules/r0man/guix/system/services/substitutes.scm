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
                                     (substitute-urls
                                      '("https://cache-cdn.guix.moe")))))))
   (default-value #f)
   (description
    "Add the guix.moe CDN mirror to the guix-daemon substitute URLs.  Since
2026-09 the mirror serves substitutes from bordeaux.guix.gnu.org,
ci.guix.gnu.org and substitutes.nonguix.org with their original signatures,
so no additional signing keys are authorized.")))
