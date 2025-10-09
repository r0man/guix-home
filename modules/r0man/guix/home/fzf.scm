(define-module (r0man guix home fzf)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services)
  #:use-module (gnu packages terminals)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:export (home-fzf-configuration
            home-fzf-service-type))

;;; Commentary:
;;;
;;; Home service for fzf fuzzy finder configuration.
;;; Adds fzf bash integration (key bindings and completion) and
;;; configures FZF_DEFAULT_OPTS environment variable.
;;;
;;; Code:

(define-record-type* <home-fzf-configuration>
  home-fzf-configuration make-home-fzf-configuration
  home-fzf-configuration?
  (default-opts home-fzf-default-opts
                (default "--bind=ctrl-j:accept")
                (description "FZF_DEFAULT_OPTS environment variable value."))
  (packages home-fzf-packages
            (default (list fzf))
            (description "List of fzf-related packages to install.")))

(define (home-fzf-bash-extension config)
  "Return bash extension for fzf bindings and completion."
  (home-bash-extension
   (bashrc
    (list (file-append fzf "/etc/bashrc.d/fzf-bindings.bash")
          (file-append fzf "/etc/bashrc.d/fzf-completion.bash")))
   (environment-variables
    `(("FZF_DEFAULT_OPTS" . ,(home-fzf-default-opts config))))))

(define (home-fzf-profile-packages config)
  "Return list of fzf packages to install."
  (home-fzf-packages config))

(define home-fzf-service-type
  (service-type
   (name 'home-fzf)
   (extensions
    (list (service-extension home-bash-service-type
                             home-fzf-bash-extension)
          (service-extension home-profile-service-type
                             home-fzf-profile-packages)))
   (default-value (home-fzf-configuration))
   (description
    "Install and configure fzf fuzzy finder with bash integration.")))
