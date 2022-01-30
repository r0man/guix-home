;; This "home-environment" file can be passed to 'guix home reconfigure'
;; to reproduce the content of your profile.  This is "symbolic": it only
;; specifies package names.  To reproduce the exact same profile, you also
;; need to capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

(define-module (r0man home config)
  #:use-module (gnu home)
  #:use-module (gnu services)
  #:use-module (r0man home bash)
  #:use-module (gnu home services)
  ;; #:use-module (r0man home shepherd)
  ;; #:use-module (r0man home mcron)
  #:use-module (r0man home packages))

(define home-environment-variables-service
  (simple-service
   'home-fix-load-path-service
   home-environment-variables-service-type
   `(;; JAVA has some issues with non reparenting window managers
     ("_JAVA_AWT_WM_NONREPARENTING" . #t)
     ;; Fix: no code for module (guix i18n) in on-first-login :/
     ("GUILE_LOAD_PATH" . "$HOME/.config/guix/current/share/guile/site/3.0:$GUILE_LOAD_PATH"))))

(home-environment
 (packages packages)
 (services (list home-environment-variables-service
                 home-bash-service
                 ;; home-mcron-service
                 ;; home-shepherd-service
                 )))
