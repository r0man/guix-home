(define-module (r0man guix home eca)
  #:use-module (gnu home services)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (r0man guix packages clojure)
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
  (package home-eca-package
           (default editor-code-assistant)
           (description "Package to install for eca."))
  (agents-file home-eca-agents-file
               (default (local-file "files/eca/AGENTS.md"))
               (description "Path to AGENTS.md file."))
  (commands-dir home-eca-commands-dir
                (default (local-file "files/eca/commands" #:recursive? #t))
                (description "Path to commands directory."))
  (config-file home-eca-config-file
               (default (local-file "files/eca/config.json"))
               (description "Path to config.json file.")))

(define (make-eca-authinfo-script config)
  "Create an executable eca-server-authinfo script."
  (program-file "eca-server-authinfo"
                #~(begin
                    (use-modules (ice-9 match))
                    (let ((args (cdr (command-line))))
                      (system (string-append
                               "set -e\n"
                               "export ECA_CONFIG='{\"netrcFile\": \"/tmp/eca-authinfo.$$\"}'\n"
                               #$(file-append gnupg "/bin/gpg")
                               " --batch -q -d ~/.authinfo.gpg > /tmp/eca-authinfo.$$\n"
                               "chmod 600 /tmp/eca-authinfo.$$\n"
                               "(sleep 5; shred -u /tmp/eca-authinfo.$$ 2>/dev/null || true) &\n"
                               "exec "
                               #$(file-append (home-eca-package config) "/bin/eca")
                               " server "
                               (string-join (map (lambda (arg)
                                                   (string-append "'" arg "'"))
                                                 args)
                                            " ")
                               "\n"))))))

(define (home-eca-files config)
  "Return alist of eca configuration files to deploy."
  `((".config/eca/AGENTS.md" ,(home-eca-agents-file config))
    (".config/eca/commands" ,(home-eca-commands-dir config))
    (".config/eca/config.json" ,(home-eca-config-file config))
    (".local/bin/eca-server-authinfo" ,(make-eca-authinfo-script config))))

(define (home-eca-profile config)
  "Return list of packages to install for eca."
  (list (home-eca-package config)))

(define home-eca-service-type
  (service-type
   (name 'home-eca)
   (extensions
    (list (service-extension home-files-service-type
                             home-eca-files)
          (service-extension home-profile-service-type
                             home-eca-profile)))
   (default-value (home-eca-configuration))
   (description
    "Install and configure Editor Code Assistant (eca) for the user.")))
