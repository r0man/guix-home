(define-module (r0man guix home environment)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (guix records)
  #:export (home-environment-configuration
            home-environment-service-type
            ;; Backward compatibility
            home-environment-variables-services))

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
             (default `(("EDITOR" . "emacsclient")
                        ("GUILE_LOAD_PATH" . "$HOME/.config/guix/current/share/guile/site/3.0:$GUILE_LOAD_PATH")
                        ("GUILE_LOAD_COMPILED_PATH" . "$HOME/.config/guix/current/lib/guile/3.0/site-ccache:$GUILE_LOAD_COMPILED_PATH")
                        ("HISTCONTROL" . "ignoredups")
                        ("HISTFILESIZE" . "10000000")
                        ("HISTSIZE" . "100000")
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

;; Backward compatibility: keep old service list export
(define home-environment-variables-services
  (list (service home-environment-service-type)))
