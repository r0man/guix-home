(define-module (r0man guix system services guix-publish)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:export (%guix-publish-service))

(define %guix-publish-service
  (service guix-publish-service-type
           (guix-publish-configuration
            (compression '(("zstd" 3)))
            (port 8082))))
