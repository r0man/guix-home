(define-module (r0man guix home environments gascity)
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
  #:use-module (r0man guix home services gascity)
  #:use-module (r0man guix home tmux)
  #:use-module (r0man guix packages claude))

;;; Commentary:
;;;
;;; Two-instance test environment for home-gascity-service-type.
;;;
;;; Instance 'main' (default beads provider 'bd, gc-home .gc-test) holds
;;; the original single-city test ('cities/test' with the beads.el rig)
;;; and an additional managed-opt-out city ('cities/test-opt-out') that
;;; exercises (managed? #f).
;;;
;;; Instance 'test2' (file-beads, gc-home .gc-test2, supervisor on
;;; 9372, dashboard on 18080) carries a single managed city with a
;;; pack and an inline agent — exercising the city.toml render path,
;;; the file-beads bootstrap (<city>/.gc/file-beads-layout +
;;; <city>/.gc/beads.json), the dashboard shepherd service, and
;;; non-default supervisor port plumbing.
;;;
;;; Usage:
;;;   modules/r0man/guix/home/files/bin/container-gascity --network
;;;
;;; Code:

(define-public gascity-home-environment
  (home-environment
   (packages (list coreutils coreutils-minimal findutils git inetutils kitty
                   ncurses claude-code nss-certs procps waypipe))
   (services
    (append (list (service home-bash-service-type
                           home-bash-default-configuration)
                  (service home-channels-service-type
                           home-channels-default-list)
                  (service home-emacs-service-type)
                  (service home-environment-service-type)
                  ;; Intentionally NOT enabling home-git-service-type:
                  ;; that service deploys ~/.gitconfig as a read-only
                  ;; symlink into the Guix store, which blocks beads'
                  ;; 'git config --global beads.role maintainer' during
                  ;; city init.  Leaving ~/.gitconfig absent lets beads
                  ;; create it on first write, and home-gascity-activation
                  ;; seeds ~/.dolt/config_global.json precisely because
                  ;; ~/.gitconfig is missing.
                  (simple-service 'fix-container-ptmx
                                  home-activation-service-type
                                  #~(when (file-exists? "/dev/pts/ptmx")
                                      (chmod "/dev/pts/ptmx" #o666)))
                  ;; 'guix home container' has no /bin/sh, but inherits
                  ;; SHELL=/bin/sh.  Point SHELL at the bash shipped in the
                  ;; profile so 'guix shell' (and anything else that exec's
                  ;; $SHELL) works inside the container.
                  (simple-service 'gascity-shell-env
                                  home-environment-variables-service-type
                                  `(("SHELL" . "$HOME/.guix-home/profile/bin/bash")))
                  (simple-service 'install-container-gascity
                                  home-files-service-type
                                  `(("bin/container-gascity"
                                     ,(local-file
                                       "../files/bin/container-gascity"))))

                  (service home-gascity-service-type
                   (home-gascity-configuration
                    (instances
                     (list
                      (gascity-instance-configuration
                       (name 'main)
                       (gc-home ".gc-test")
                       (cities
                        (list (gascity-city-configuration
                               (path (string-append (getenv "HOME")
                                                    "/cities/test"))
                               (rigs
                                (list (gascity-rig-configuration
                                       (path "rigs/beads.el")
                                       (git-url "https://github.com/r0man/beads.el")
                                       (branch "main")
                                       (depth 1)))))
                              (gascity-city-configuration
                               (path (string-append (getenv "HOME")
                                                    "/cities/test-opt-out"))
                               (managed? #f)))))
                      (gascity-instance-configuration
                       (name 'test2)
                       (gc-home ".gc-test2")
                       (beads-provider 'file)
                       (supervisor-port 9372)
                       (dashboard? #t)
                       (dashboard-port 18080)
                       (cities
                        (list (gascity-city-configuration
                               (path (string-append (getenv "HOME")
                                                    "/cities/test2"))
                               (packs
                                (list (gascity-pack-configuration
                                       (name "examples")
                                       (source "https://github.com/gastownhall/gascity")
                                       (path "examples/packs"))))
                               (agents
                                (list (gascity-agent-configuration
                                       (name "watcher")
                                       (provider "claude")))))))))))))
            home-tmux-services))))

gascity-home-environment
