(define-module (r0man guix home bash)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services)
  #:use-module (gnu home)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:export (home-bash-services))

(define %bash-rc
  (local-file "files/bashrc" "bash.rc"))

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
           (list %bash-rc))
          (bash-profile
           (list (local-file "files/bash_profile" "bash_profile")))
          (bash-logout
           (list (local-file "files/bash_logout" "bash_logout")))))
        (simple-service 'bash-packages home-profile-service-type
                        (list vivid))))
