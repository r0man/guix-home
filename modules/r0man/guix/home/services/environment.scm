(define-module (r0man guix home services environment)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (guix records)
  #:export (home-environment-configuration
            home-environment-service-type))

;;; Commentary:
;;;
;;; Home service for environment variables configuration.
;;; Manages shell environment variables.
;;;
;;; Code:

(define-record-type* <home-environment-configuration>
  home-environment-configuration make-home-environment-configuration
  home-environment-configuration?
  (variables home-environment-variables
             ;; NOTE: GNUPGHOME is re-declared here to override the one set
             ;; by home-gpg-agent-service-type.  Upstream resolves it with
             ;; (getenv "HOME") at evaluation time, so when the home
             ;; environment is instantiated from the system configuration via
             ;; guix-home-service-type, `guix system reconfigure' runs as root
             ;; and bakes in "/root/.gnupg".  Using "$HOME" defers expansion to
             ;; the shell sourcing setup-environment.
             (default `(("GNUPGHOME" . "$HOME/.gnupg")
                        ("EDITOR" . "emacsclient")
                        ("HISTCONTROL" . "ignoredups")
                        ("HISTFILESIZE" . "10000000")
                        ("HISTSIZE" . "100000")
                        ("ELECTRON_OZONE_PLATFORM_HINT" . "auto")
                        ("GTK_IM_MODULE" . "none")
                        ("VISUAL" . "emacsclient")
                        ("_JAVA_AWT_WM_NONREPARENTING" . #t)))
             (description "Alist of environment variables to set.")))

(define (home-environment-vars config)
  "Return alist of environment variables to set."
  (home-environment-variables config))

(define home-environment-service-type
  (service-type
   (name 'home-environment)
   (extensions
    (list (service-extension home-environment-variables-service-type
                             home-environment-vars)))
   (default-value (home-environment-configuration))
   (description
    "Set user environment variables.")))
