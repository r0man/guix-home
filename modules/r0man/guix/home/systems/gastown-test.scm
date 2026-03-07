(define-module (r0man guix home systems gastown-test)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu home)
  #:use-module (gnu packages base)
  #:use-module (gnu packages version-control)
  #:use-module (gnu services)
  #:use-module (r0man guix home services gastown)
  #:use-module (r0man guix services gastown))

;;; Commentary:
;;;
;;; Minimal home environments for testing home-gastown-service-type with
;;; three independent town roots and rig/crew configurations.
;;;
;;; Usage:
;;;   scripts/test-gastown-towns.sh
;;;
;;; Code:

(define (make-test-env town-root rigs)
  "Return a minimal home-environment for testing with TOWN-ROOT and RIGS."
  (home-environment
   (packages (list coreutils git))
   (services
    (list (service home-gastown-service-type
                   (home-gastown-configuration
                    (town-root town-root)
                    (rigs rigs)))))))

;;; Three independent town/rig configurations — each uses a different town-root
;;; so their data directories never collide.

(define-public gastown-test-env-1
  (make-test-env
   "town1"
   (list (gastown-rig-configuration
          (name "repo-a")
          (git-url "git@github.com:r0man/guix-home")
          (prefix "ra")
          (crews (list (gastown-crew-configuration (name "roman"))))))))

(define-public gastown-test-env-2
  (make-test-env
   "town2"
   (list (gastown-rig-configuration
          (name "repo-b")
          (git-url "git@github.com:r0man/guix-channel")
          (prefix "rb")
          (crews (list (gastown-crew-configuration (name "roman"))
                       (gastown-crew-configuration (name "alice"))))))))

(define-public gastown-test-env-3
  (make-test-env
   "town3"
   (list (gastown-rig-configuration
          (name "repo-c")
          (git-url "git@github.com:r0man/beads.el")
          (prefix "rc"))
         (gastown-rig-configuration
          (name "repo-d")
          (git-url "git@github.com:r0man/gastown.el")
          (prefix "rd")
          (crews (list (gastown-crew-configuration (name "roman"))))))))

;; Default export: first env (for direct `guix home container` use).
gastown-test-env-1
