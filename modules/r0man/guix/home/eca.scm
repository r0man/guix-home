(define-module (r0man guix home eca)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:export (home-eca-configuration
            home-eca-service-type))

;;; Commentary:
;;;
;;; Home service for Editor Code Assistant (eca) configuration.
;;; Manages eca config files in ~/.config/eca/
;;;
;;; Code:

(define-record-type* <home-eca-configuration>
  home-eca-configuration make-home-eca-configuration
  home-eca-configuration?
  (agents-file home-eca-agents-file
               (default (local-file "files/eca/AGENTS.md"))
               (description "Path to AGENTS.md file."))
  (commands-dir home-eca-commands-dir
                (default (local-file "files/eca/commands" #:recursive? #t))
                (description "Path to commands directory."))
  (config-file home-eca-config-file
               (default (local-file "files/eca/config.json"))
               (description "Path to config.json file.")))

(define (home-eca-files config)
  "Return alist of eca configuration files to deploy."
  `((".config/eca/AGENTS.md" ,(home-eca-agents-file config))
    (".config/eca/commands" ,(home-eca-commands-dir config))
    (".config/eca/config.json" ,(home-eca-config-file config))))

(define home-eca-service-type
  (service-type
   (name 'home-eca)
   (extensions
    (list (service-extension home-files-service-type
                             home-eca-files)))
   (default-value (home-eca-configuration))
   (description
    "Install and configure Editor Code Assistant (eca) for the user.")))
