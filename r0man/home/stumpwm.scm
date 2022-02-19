(define-module (r0man home stumpwm)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:export (home-stumpwm-services))

(define files
  `(("config/stumpwm/commands.lisp" ,(local-file "files/stumpwm/commands.lisp"))
    ("config/stumpwm/config" ,(local-file "files/stumpwm/config"))
    ("config/stumpwm/polybar.lisp" ,(local-file "files/stumpwm/polybar.lisp"))
    ("config/stumpwm/time.lisp" ,(local-file "files/stumpwm/time.lisp"))))

(define home-stumpwm-services
  (list (simple-service 'stumpwm home-files-service-type files)))
