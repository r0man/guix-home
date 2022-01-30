(define-module (r0man home environment)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:export (home-environment-variables-service))

(define home-environment-variables-service
  (simple-service
   'home-environment-variables-service
   home-environment-variables-service-type
   `(;; JAVA has some issues with non reparenting window managers
     ("_JAVA_AWT_WM_NONREPARENTING" . #t)
     ;; Fix: no code for module (guix i18n) in on-first-login :/
     ("GUILE_LOAD_PATH" . "$HOME/.config/guix/current/share/guile/site/3.0:$GUILE_LOAD_PATH"))))
