(define-module (r0man guix home systems agent)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu bootloader)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services ssh)
  #:use-module (gnu home services)
  #:use-module (gnu home)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages less)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages web)
  #:use-module (gnu services base)
  #:use-module (gnu services guix)
  #:use-module (gnu services)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system shadow)
  #:use-module (gnu system)
  #:use-module (r0man guix packages clojure)
  #:use-module (r0man guix packages java)
  #:use-module (r0man guix packages task-management)
  #:use-module (r0man guix home bash)
  #:use-module (r0man guix home claude-code)
  #:use-module (r0man guix home clojure)
  #:use-module (r0man guix home environment)
  #:use-module (r0man guix home fzf)
  #:use-module (r0man guix home git)
  #:use-module (r0man guix home tmux))

;;; Commentary:
;;;
;;; Minimal home environment for an AI agent doing Clojure development.
;;; Includes Clojure tooling (leiningen, clojure-lsp, babashka),
;;; GraalVM as JDK, and Claude Code.
;;;
;;; Usage:
;;;   guix home -L modules --dry-run reconfigure \
;;;     modules/r0man/guix/home/systems/agent.scm
;;;
;;; Code:

(define services
  (append home-tmux-services
          (list (service home-bash-service-type
                         home-bash-default-configuration)
                (service home-claude-code-service-type)
                (service home-clojure-service-type)
                (service home-environment-service-type)
                (service home-fzf-service-type)
                (service home-git-service-type)
                (service home-ssh-agent-service-type))))

(define packages
  (list beads-next
        clojure-tools-bin-latest
        coreutils
        curl
        diffutils
        fd
        findutils
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

(define-public agent-home-environment
  (home-environment
   (packages (append packages home-bash-packages))
   (services services)))

(define-public agent-operating-system
  (operating-system
    (host-name "agent")
    (bootloader (bootloader-configuration
                 (bootloader grub-efi-removable-bootloader)
                 (targets (list "/boot/efi"))))
    (file-systems (list (file-system
                          (mount-point "/")
                          (device "none")
                          (type "tmpfs")
                          (check? #f))))
    (users (cons (user-account
                  (name "roman")
                  (group "users")
                  (home-directory "/home/roman"))
                 %base-user-accounts))
    (services (append
               (list (service guix-home-service-type
                       `(("roman" ,agent-home-environment))))
               %base-services))))

agent-home-environment
