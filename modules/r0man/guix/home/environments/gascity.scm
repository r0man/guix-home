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
;;; Minimal home environment for testing home-gascity-service-type.
;;; Configures a single city at ~/cities/test with the beads.el rig,
;;; runs the singleton supervisor under shepherd with GC_HOME=~/.gc-test,
;;; and installs the container-gascity helper script at $HOME/bin/.
;;;
;;; Usage:
;;;   modules/r0man/guix/home/files/bin/container-gascity --network
;;;
;;; Code:

(define-public gascity-home-environment
  (home-environment
   (packages (list coreutils coreutils-minimal git inetutils kitty ncurses
                   claude-code nss-certs procps waypipe))
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
                  (simple-service 'install-container-gascity
                                  home-files-service-type
                                  `(("bin/container-gascity"
                                     ,(local-file
                                       "../files/bin/container-gascity"))))

                  (service home-gascity-service-type
                   (home-gascity-configuration
                    (gc-home ".gc-test")
                    (cities
                     (list (gascity-city-configuration
                            (path (string-append (getenv "HOME")
                                                 "/cities/test"))
                            (rigs (list (gascity-rig-configuration
                                         (path "rigs/beads.el")
                                         (git-url "https://github.com/r0man/beads.el"))))))))))
            home-tmux-services))))

gascity-home-environment
