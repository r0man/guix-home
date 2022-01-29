;; This "home-environment" file can be passed to 'guix home reconfigure'
;; to reproduce the content of your profile.  This is "symbolic": it only
;; specifies package names.  To reproduce the exact same profile, you also
;; need to capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

(define-module (r0man home config)
  #:use-module (gnu home)
  #:use-module (r0man home bash)
  #:use-module (r0man home packages))

(home-environment
 (packages packages)
 (services (list home-bash-service)))
