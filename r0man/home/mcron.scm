(define-module (r0man home mcron)
  #:use-module (gnu home services mcron)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:export (home-mcron-service))

(define guix-garbage-collect
  #~(job "0 12 * * *" "guix gc --delete-generations=1m --free-space=10G"))

(define home-mcron-service
  (service
   home-mcron-service-type
   (home-mcron-configuration
    (jobs (list guix-garbage-collect)))))
