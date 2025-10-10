(define-module (r0man guix home bash)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services)
  #:use-module (gnu packages rust-apps)
  #:use-module (guix gexp)
  #:export (home-bash-default-configuration
            home-bash-default-packages))

;;; Commentary:
;;;
;;; This module provides pre-configured home-bash-configuration
;;; instances for use with the upstream home-bash-service-type.
;;;
;;; The configuration includes:
;;; - Standard bash aliases for navigation and applications
;;; - Custom bashrc, bash_profile, and bash_logout files
;;; - Bash-related packages (vivid for LS_COLORS)
;;;
;;; Usage in system configs:
;;;   (service home-bash-service-type home-bash-default-configuration)
;;;   (simple-service 'bash-packages home-profile-service-type
;;;                   home-bash-default-packages)
;;;
;;; Code:

(define home-bash-default-configuration
  (home-bash-configuration
   (aliases '((".." . "cd ..")
              ("..." . "cd .. && cd..")
              ("e" . "emacsclient")
              ("ls" . "ls --color=auto")
              ("la" . "ls -lha")
              ("ll" . "ls -lh")
              ("chromium" . "flatpak run org.chromium.Chromium")
              ("firefox" . "flatpak run org.mozilla.firefox")
              ("slack" . "flatpak run com.slack.Slack")
              ("zoom" . "flatpak run us.zoom.Zoom")))
   (bashrc (list (local-file "files/bashrc" "bash.rc")))
   (bash-profile (list (local-file "files/bash_profile" "bash_profile")))
   (bash-logout (list (local-file "files/bash_logout" "bash_logout")))))

(define home-bash-default-packages
  (list vivid))
