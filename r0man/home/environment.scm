(define-module (r0man home environment)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:export (home-environment-variables-services))

(define home-environment-variables-services
  (list (simple-service
         'home-environment-variables-service
         home-environment-variables-service-type
         `(("EDITOR" . "emacsclient")
           ("HISTCONTROL" . "ignoredups")
           ("HISTFILESIZE" . "10000")
           ("HISTSIZE" . "10000")
           ("PROMPT_COMMAND" . "'history -a'")
           ("VISUAL" . "emacsclient")
           ("_JAVA_AWT_WM_NONREPARENTING" . #t)))))
