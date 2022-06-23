(define-module (r0man home profiles)
  #:use-module (gnu home services)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (gnu packages wm)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (r0man packages lisp)
  #:export (home-profiles-services))

(define files
  `((".config/guix/r0man/profile/emacs.scm" ,(local-file "../profile/emacs.scm"))))

(define home-profiles-services
  (list (simple-service 'profiles-config home-files-service-type files)))
