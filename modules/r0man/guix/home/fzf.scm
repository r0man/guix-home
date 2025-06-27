(define-module (r0man guix home fzf)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services)
  #:use-module (gnu packages terminals)
  #:use-module (guix gexp)
  #:export (home-fzf-services))

(define %home-bash
  (home-bash-extension
   (bashrc
    (list (file-append fzf "/etc/bashrc.d/fzf-bindings.bash")
          (file-append fzf "/etc/bashrc.d/fzf-completion.bash")))
   (environment-variables
    '(("FZF_DEFAULT_OPTS" . "--bind=ctrl-j:accept")))))

(define %home-profile
  (list fzf))

(define home-fzf-services
  (list (simple-service 'fzf-home-bash home-bash-service-type %home-bash)
        (simple-service 'fzf-home-profile home-profile-service-type %home-profile)))
