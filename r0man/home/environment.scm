(define-module (r0man home environment)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:export (home-environment-variables-services))

(define home-environment-variables-services
  (list (simple-service
         'home-environment-variables-service
         home-environment-variables-service-type
         `(("EDITOR" . "emacsclient")
           ("GUILE_LOAD_PATH" . "$HOME/.config/guix/current/share/guile/site/3.0:$GUILE_LOAD_PATH") ;; Fix: no code for module (guix i18n) in on-first-login :/
           ("HISTCONTROL" . "ignoredups")
           ("HISTFILESIZE" . "10000000")
           ("HISTSIZE" . "100000")
           ("VISUAL" . "emacsclient")
           ("_JAVA_AWT_WM_NONREPARENTING" . #t)))))
