(define-module (r0man guix home environments gascity)
  #:use-module (gnu home services guix)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services)
  #:use-module (gnu home)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gawk)
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
;;; Single-supervisor container environment for home-gascity-service-type:
;;;
;;;   gascity-supervisor-downtown   (bd, has the beads.el rig)
;;;   gascity-init-downtown         (one-shot bootstrap)
;;;
;;; 'downtown' — managed, default 'bd' beads, gc-home .gc-downtown,
;;;              supervisor on 28371 (non-default: the container runs
;;;              with --network and the host typically binds 8372 for
;;;              its own gascity supervisor).  Hosts the beads.el rig.
;;;
;;; Usage:
;;;   modules/r0man/guix/home/files/bin/container-gascity
;;;   modules/r0man/guix/home/files/bin/container-gascity -- emacs
;;;
;;; Code:

(define-public gascity-home-environment
  (home-environment
   (packages (list coreutils coreutils-minimal emacs-pgtk findutils gawk
                   git inetutils kitty ncurses claude-code nss-certs procps
                   waypipe))
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
                    (supervisors
                     (list
                      (gascity-supervisor-configuration
                       (name 'downtown)
                       (gc-home ".gc-downtown")
                       (supervisor-port 28371)
                       (cities
                        (list (gascity-city-configuration
                               (path (string-append (getenv "HOME")
                                                    "/cities/downtown"))
                               (rigs
                                (list (gascity-rig-configuration
                                       (path "rigs/beads.el")
                                       (git-url "https://github.com/r0man/beads.el")
                                       (branch "main")
                                       (depth 1)))))))))))))
            home-tmux-services))))

gascity-home-environment
