(define-module (r0man guix home bash)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services)
  #:use-module (gnu home)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:export (home-bash-custom-configuration
            make-home-bash-services
            ;; Backward compatibility
            home-bash-services))

;;; Commentary:
;;;
;;; Home service for Bash shell configuration.
;;; Manages bash aliases, rc files, and packages.
;;;
;;; Code:

(define-record-type* <home-bash-custom-configuration>
  home-bash-custom-configuration make-home-bash-custom-configuration
  home-bash-custom-configuration?
  (aliases home-bash-aliases
           (default '((".." . "cd ..")
                      ("..." . "cd .. && cd..")
                      ("e" . "emacsclient")
                      ("ls" . "ls --color=auto")
                      ("la" . "ls -lha")
                      ("ll" . "ls -lh")
                      ("chromium" . "flatpak run org.chromium.Chromium")
                      ("firefox" . "flatpak run org.mozilla.firefox")
                      ("slack" . "flatpak run com.slack.Slack")
                      ("zoom" . "flatpak run us.zoom.Zoom")))
           (description "Alist of bash aliases."))
  (bashrc-file home-bash-bashrc-file
               (default (local-file "files/bashrc" "bash.rc"))
               (description "Bash rc configuration file."))
  (bash-profile-file home-bash-bash-profile-file
                     (default (local-file "files/bash_profile" "bash_profile"))
                     (description "Bash profile configuration file."))
  (bash-logout-file home-bash-bash-logout-file
                    (default (local-file "files/bash_logout" "bash_logout"))
                    (description "Bash logout configuration file."))
  (packages home-bash-packages
            (default (list vivid))
            (description "List of Bash-related packages to install.")))

(define* (make-home-bash-services #:optional
                                  (config (home-bash-custom-configuration)))
  "Create list of bash services from configuration."
  (list (service home-bash-service-type
                 (home-bash-configuration
                  (aliases (home-bash-aliases config))
                  (bashrc (list (home-bash-bashrc-file config)))
                  (bash-profile (list (home-bash-bash-profile-file config)))
                  (bash-logout (list (home-bash-bash-logout-file config)))))
        (simple-service 'bash-packages
                        home-profile-service-type
                        (home-bash-packages config))))

;; Backward compatibility: keep old service list export
(define home-bash-services
  (make-home-bash-services))
