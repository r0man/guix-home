(define-module (r0man guix home common-lisp)
  #:use-module (gnu home services)
  #:use-module (gnu packages lisp)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:export (home-common-lisp-services))

(define files
  `((".sbclrc" ,(local-file "files/sbclrc"))))

(define packages
  (list sbcl))

(define home-common-lisp-services
  (list (simple-service 'common-lisp-files home-files-service-type files)
        (simple-service 'common-lisp-packages home-profile-service-type packages)))
