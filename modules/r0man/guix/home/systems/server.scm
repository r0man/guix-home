(define-module (r0man guix home systems server)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services)
  #:use-module (gnu home)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (r0man guix home bash)
  #:use-module (r0man guix home btop)
  #:use-module (r0man guix home channels)
  #:use-module (r0man guix home clojure)
  #:use-module (r0man guix home common-lisp)
  #:use-module (r0man guix home eca)
  #:use-module (r0man guix home emacs)
  #:use-module (r0man guix home environment)
  #:use-module (r0man guix home fzf)
  #:use-module (r0man guix home git)
  #:use-module (r0man guix home gpg)
  #:use-module (r0man guix home guile)
  #:use-module (r0man guix home mbsync)
  #:use-module (r0man guix home msmtp)
  #:use-module (r0man guix home nix)
  #:use-module (r0man guix home packages)
  #:use-module (r0man guix home pm)
  #:use-module (r0man guix home shepherd)
  #:use-module (r0man guix home ssh))

(define services
  (append (make-home-bash-services)
          (make-home-channels-services)
          (make-home-gpg-services
           (home-gpg-custom-configuration
            (pinentry-program (file-append pinentry-tty "/bin/pinentry"))))
          (make-home-pm-services)
          home-msmtp-services
          home-shepherd-services
          home-ssh-services
          (list (service home-btop-service-type)
                (service home-clojure-service-type)
                (service home-common-lisp-service-type)
                (service home-dbus-service-type)
                (service home-eca-service-type)
                (service home-emacs-service-type)
                (service home-environment-service-type)
                (service home-fzf-service-type)
                (service home-git-service-type)
                (service home-guile-service-type)
                (service home-mbsync-service-type)
                (service home-nix-service-type))))

(define-public server-home-environment
  (home-environment
   (packages packages-base)
   (services services)))

server-home-environment
