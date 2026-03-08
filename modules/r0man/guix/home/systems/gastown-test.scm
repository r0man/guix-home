(define-module (r0man guix home systems gastown-test)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu home)
  #:use-module (gnu packages base)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages version-control)
  #:use-module (gnu services)
  #:use-module (r0man guix home services gastown)
  #:use-module (r0man guix packages node)
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

(define* (make-test-env name town-root rigs #:key (dolt-port 3307))
  "Return a minimal home-environment for testing with NAME, TOWN-ROOT, RIGS
and optional DOLT-PORT (default 3307)."
  (home-environment
   (packages (list coreutils coreutils-minimal git
                   node-anthropic-ai-claude-code nss-certs procps))
   (services
    (list (service home-gastown-service-type
                   (home-gastown-configuration
                    (towns (list (gastown-town-configuration
                                  (name name)
                                  (town-root town-root)
                                  (dolt (gastown-dolt-configuration
                                         (port dolt-port)))
                                  (rigs rigs))))))))))

;;; Three independent town/rig configurations — each uses a different town-root
;;; so their data directories never collide.

(define-public gastown-test-env-1
  (make-test-env
   "town1" "town1"
   (list (gastown-rig-configuration
          (name "repo_a")
          (git-url "https://github.com/r0man/guix-home")
          (prefix "ra")
          (crews (list (gastown-crew-configuration (name "roman"))))))
   #:dolt-port 13307))

(define-public gastown-test-env-2
  (make-test-env
   "town2" "town2"
   (list (gastown-rig-configuration
          (name "repo_b")
          (git-url "https://github.com/r0man/guix-channel")
          (prefix "rb")
          (crews (list (gastown-crew-configuration (name "roman"))
                       (gastown-crew-configuration (name "alice"))))))
   #:dolt-port 13308))

(define-public gastown-test-env-3
  (make-test-env
   "town3" "town3"
   (list (gastown-rig-configuration
          (name "repo_c")
          (git-url "https://github.com/r0man/beads.el")
          (prefix "rc"))
         (gastown-rig-configuration
          (name "repo_d")
          (git-url "https://github.com/r0man/gastown.el")
          (prefix "rd")
          (crews (list (gastown-crew-configuration (name "roman"))))))
   #:dolt-port 13309))

;;; Multi-town test: two towns in a single home environment, each with its
;;; own dolt port, rig, and crew configuration.

(define-public gastown-test-env-multi
  (home-environment
   (packages (list coreutils coreutils-minimal git
                   node-anthropic-ai-claude-code nss-certs procps))
   (services
    (list (service home-gastown-service-type
                   (home-gastown-configuration
                    (towns (list (gastown-town-configuration
                                  (name "alpha")
                                  (town-root "alpha")
                                  (dolt (gastown-dolt-configuration
                                         (port 13307)))
                                  (rigs (list (gastown-rig-configuration
                                               (name "repo_a")
                                               (git-url "https://github.com/r0man/guix-home")
                                               (prefix "ra")
                                               (crews (list (gastown-crew-configuration
                                                              (name "roman"))))))))
                                 (gastown-town-configuration
                                  (name "beta")
                                  (town-root "beta")
                                  (dolt (gastown-dolt-configuration
                                         (port 13308)))
                                  (rigs (list (gastown-rig-configuration
                                               (name "repo_b")
                                               (git-url "https://github.com/r0man/guix-channel")
                                               (prefix "rb")
                                               (crews (list (gastown-crew-configuration
                                                              (name "roman"))
                                                            (gastown-crew-configuration
                                                              (name "alice"))))))))))))))))

;; Default export: multi-town env (for direct `guix home container` use).
gastown-test-env-multi
