(define-module (r0man home common-lisp)
  #:use-module (gnu home services)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages lisp-check)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:export (home-common-lisp-services))

(define files
  `())

(define packages
  (list cl-annot
        cl-clack
        cl-project
        cl-rove))

(define home-common-lisp-services
  (list (simple-service 'common-lisp-files home-files-service-type files)
        (simple-service 'common-lisp-packages home-profile-service-type packages)))
