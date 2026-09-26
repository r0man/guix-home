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
;;; ~/.pi/agent/settings.json, models.json and auth.json are left alone:
;;; they hold provider state and secrets.
;;;
;;; Code:

(define-record-type* <home-pi-configuration>
  home-pi-configuration make-home-pi-configuration
  home-pi-configuration?
  (skills home-pi-skills
          (default %agent-skills)
          (description "Path to skills directory."))
  (packages home-pi-packages
            (default (list pi-coding-agent))
            (description "List of pi packages to install.")))

(define (home-pi-files config)
  "Return alist of pi configuration files to deploy."
  `((".pi/agent/skills" ,(home-pi-skills config))))

(define (home-pi-profile-packages config)
  "Return list of pi packages to install."
  (home-pi-packages config))

(define home-pi-service-type
  (service-type
   (name 'home-pi)
   (extensions
    (list (service-extension home-files-service-type
                             home-pi-files)
          (service-extension home-profile-service-type
                             home-pi-profile-packages)))
   (default-value (home-pi-configuration))
   (description
    "Install and configure the pi coding agent for the user.")))
