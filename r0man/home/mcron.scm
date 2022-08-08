(define-module (r0man home mcron)
  #:use-module (gnu home services mcron)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:export (home-mcron-services))

(define guix-garbage-collect
  #~(job "0 12 * * *" "guix gc --delete-generations=1m"))

(define mbsync
  #~(job "* * * * *" "mbsync --all --verbose"))

(define jobs
  (list mbsync))

(define home-mcron-services
  (list (service home-mcron-service-type
                 (home-mcron-configuration (jobs jobs)))))
