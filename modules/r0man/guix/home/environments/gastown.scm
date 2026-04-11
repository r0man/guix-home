(define-module (r0man guix home environments gastown)
  #:use-module (gnu home services guix)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services)
  #:use-module (gnu home)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages version-control)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (r0man guix home bash)
  #:use-module (r0man guix home channels)
  #:use-module (r0man guix home services emacs)
  #:use-module (r0man guix home services environment)
  #:use-module (r0man guix home services gastown)
  #:use-module (r0man guix home tmux)
  #:use-module (r0man guix packages node)
  #:use-module (r0man guix services gastown))

;;; Commentary:
;;;
;;; Minimal home environment for testing home-gastown-service-type with a
;;; single town at ~/gt-test and two rigs: beads.el and gastown.el.
;;;
;;; Usage:
;;;   guix home -L modules container --network modules/r0man/guix/home/environments/gastown.scm
;;;
;;; Code:

(define-public gastown-home-environment
  (home-environment
   (packages (list coreutils coreutils-minimal git inetutils kitty ncurses
                   node-anthropic-ai-claude-code nss-certs procps waypipe))
   (services
    (append (list (service home-bash-service-type
                           home-bash-default-configuration)
                  (service home-channels-service-type
                           home-channels-default-list)
                  (service home-emacs-service-type)
                  (service home-environment-service-type)
                  (simple-service 'fix-container-ptmx
                                  home-activation-service-type
                                  #~(when (file-exists? "/dev/pts/ptmx")
                                      (chmod "/dev/pts/ptmx" #o666)))

                  (service home-gastown-service-type
                   (home-gastown-configuration
                    (towns (list (gastown-town-configuration
                                  (name "test")
                                  (town-root "gt-test")
                                  (dolt (gastown-dolt-configuration
                                         (port 13307)))
                                  (rigs (list (gastown-rig-configuration
                                               (name "beads_el")
                                               (git-url "https://github.com/r0man/beads.el")
                                               (prefix "be"))
                                              (gastown-rig-configuration
                                               (name "gastown_el")
                                               (git-url "https://github.com/r0man/gastown.el")
                                               (prefix "ge"))))))))))
            home-tmux-services))))

gastown-home-environment
