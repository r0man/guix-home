(define-module (r0man home bash)
  #:use-module (gnu home services shells)
  #:use-module (gnu home)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:export (home-bash-services))

(define home-bash-services
  (list (service
         home-bash-service-type
         (home-bash-configuration
          (aliases
           '((".." . "cd ..")
             ("..." . "cd .. && cd..")
             ("e" . "emacsclient")
             ("la" . "ls -lha")
             ("ll" . "ls -lh")))
          (bashrc
           (list (local-file "files/bash/.bashrc" "bashrc")))
          (bash-profile
           (list (local-file "files/bash/.bash_profile" "bash_profile")))
          (bash-logout
           (list (local-file "files/bash/.bash_logout" "bash_logout")))
          (environment-variables
           '(("HISTCONTROL" . "ignoredups")
             ("HISTFILESIZE" . "10000")
             ("HISTSIZE" . "10000")
             ("PROMPT_COMMAND" . "'history -a'")))))))
