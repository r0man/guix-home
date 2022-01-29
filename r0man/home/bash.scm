(define-module (r0man home bash)
  #:use-module (gnu home services shells)
  #:use-module (gnu home)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:export (home-bash-service))

(define home-bash-service
  (service
   home-bash-service-type
   (home-bash-configuration
    (aliases
     '((".." . "cd ..")
       ("..." . "cd .. && cd..")
       ("e" . "emacsclient")
       ("la" . "ls -lha")
       ("ll" . "ls -lh")))
    (bashrc
     (list (local-file "bash/.bashrc" "bashrc")))
    (bash-profile
     (list (local-file "bash/.bash_profile" "bash_profile")))
    (bash-logout
     (list (local-file "bash/.bash_logout" "bash_logout")))
    (environment-variables
     '(("EDITOR" . "emacsclient"))))))
