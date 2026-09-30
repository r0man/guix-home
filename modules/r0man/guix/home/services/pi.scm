(define-module (r0man guix home services pi)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (r0man guix home services agent-tools)
  #:use-module (r0man guix packages pi)
  #:export (home-pi-configuration
            home-pi-service-type))

;;; Commentary:
;;;
;;; Home service for the pi coding agent.  Manages ~/.pi/agent/skills
;;; (shared with other agents, see agent-tools.scm).
;;;
;;; ~/.pi/agent/settings.json stays a regular file, because pi writes
;;; to it (/settings, changelog state).  The activation merges
;;; files/pi/settings.json into it: managed keys win, everything else
;;; is kept.  models.json and auth.json are left alone.
;;;
;;; Code:

(define-record-type* <home-pi-configuration>
  home-pi-configuration make-home-pi-configuration
  home-pi-configuration?
  (skills home-pi-skills
          (default %agent-skills)
          (description "Path to skills directory."))
  (settings home-pi-settings
            (default (local-file "../files/pi/settings.json"
                                 "pi-settings.json"))
            (description "Settings merged into ~/.pi/agent/settings.json."))
  (packages home-pi-packages
            (default (list pi-coding-agent))
            (description "List of pi packages to install.")))

(define (home-pi-files config)
  "Return alist of pi configuration files to deploy."
  `((".pi/agent/skills" ,(home-pi-skills config))))

(define (home-pi-activation config)
  "Merge the managed settings into ~/.pi/agent/settings.json."
  (json-settings-activation ".pi/agent/settings.json"
                            (home-pi-settings config)))

(define (home-pi-profile-packages config)
  "Return list of pi packages to install."
  (home-pi-packages config))

(define home-pi-service-type
  (service-type
   (name 'home-pi)
   (extensions
    (list (service-extension home-activation-service-type
                             home-pi-activation)
          (service-extension home-files-service-type
                             home-pi-files)
          (service-extension home-profile-service-type
                             home-pi-profile-packages)))
   (default-value (home-pi-configuration))
   (description
    "Install and configure the pi coding agent for the user.")))
