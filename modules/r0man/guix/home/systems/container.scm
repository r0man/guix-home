(define-module (r0man guix home systems container)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services ssh)
  #:use-module (gnu home services)
  #:use-module (gnu home)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages less)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages web)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages version-control)
  #:use-module (gnu services)
  #:use-module (r0man guix packages clojure)
  #:use-module (r0man guix packages java)
  #:use-module (r0man guix packages task-management)
  #:use-module (r0man guix home bash)
  #:use-module (r0man guix home claude-code)
  #:use-module (r0man guix home clojure)
  #:use-module (r0man guix home emacs)
  #:use-module (r0man guix home environment)
  #:use-module (r0man guix home fzf)
  #:use-module (r0man guix home git)
  #:use-module (r0man guix home ssh)
  #:use-module (r0man guix home tmux))

;;; Commentary:
;;;
;;; Minimal home environment for Clojure development in containers.
;;;
;;; Usage:
;;;   guix home -L modules container \
;;;     modules/r0man/guix/home/systems/container.scm
;;;
;;; With sound/display passthrough:
;;;   guix home -L modules container \
;;;     --network \
;;;     --share=/run/user/$UID/pipewire-0 \
;;;     --share=/run/user/$UID/pulse \
;;;     --share=/tmp/.X11-unix \
;;;     --share=/run/user/$UID/wayland-0 \
;;;     --share=$SSH_AUTH_SOCK \
;;;     modules/r0man/guix/home/systems/container.scm
;;;
;;; Code:

(define services
  (append home-tmux-services
          (list (service home-bash-service-type
                         home-bash-default-configuration)
                (service home-claude-code-service-type)
                (service home-clojure-service-type)
                (service home-emacs-service-type)
                (service home-environment-service-type)
                (service home-fzf-service-type)
                (service home-git-service-type)
                (service home-openssh-service-type
                         home-openssh-default-configuration)
                (service home-ssh-agent-service-type))))

(define base-packages
  (list beads-next
        clojure-tools-bin-latest
        coreutils
        curl
        diffutils
        fd
        findutils
        font-inconsolata
        git
        glibc-locales
        graalvm-ce
        grep
        guix
        inetutils
        jq
        less
        libvterm
        gnu-make
        nss-certs
        openssh
        ripgrep
        sed
        which))

(define-public container-home-environment
  (home-environment
   (packages (append base-packages home-bash-packages))
   (services services)))

container-home-environment
