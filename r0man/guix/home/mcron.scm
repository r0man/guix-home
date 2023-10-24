(define-module (r0man guix home mcron)
  #:use-module (gnu home services mcron)
  #:use-module (gnu packages mail)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:export (home-mcron-services))

(define mbsync
  #~(job "* * * * *" (string-append #$isync "/bin/mbsync --all")))

(define jobs
  (list mbsync))

(define home-mcron-services
  (list (service home-mcron-service-type
                 (home-mcron-configuration (jobs jobs)))))
