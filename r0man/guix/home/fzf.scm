(define-module (r0man guix home fzf)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services)
  #:use-module (gnu home)
  #:use-module (gnu packages terminals)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:export (home-fzf-services))

(define %bash-completions
  (mixed-text-file "fzf-bash-completions.sh" "source " (file-append fzf "/etc/bash_completion.d/fzf")))

(define %bash-key-bindings
  (local-file "files/fzf/shell/key-bindings.bash" "fzf-bash-key-bindings.sh"))

(define %home-bash
  (home-bash-extension
   (bashrc (list %bash-completions %bash-key-bindings))))

(define %home-profile
  (list fzf))

(define home-fzf-services
  (list (simple-service 'fzf-home-bash home-bash-service-type %home-bash)
        (simple-service 'fzf-home-profile home-profile-service-type %home-profile)))
