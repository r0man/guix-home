(define-module (r0man guix home common-lisp)
  #:use-module (gnu home services)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages lisp-check)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:export (home-common-lisp-services))

(define files
  `((".sbclrc" ,(local-file "files/sbclrc"))))

(define packages
  (list cl-annot
        cl-autowrap
        cl-cffi
        cl-clack
        cl-clack
        cl-dexador
        cl-project
        cl-rdkafka
        cl-rove
        sbcl))

(define home-common-lisp-services
  (list (simple-service 'common-lisp-files home-files-service-type files)
        (simple-service 'common-lisp-packages home-profile-service-type packages)))
