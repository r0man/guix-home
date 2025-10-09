(define-module (r0man guix home nix)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:export (home-nix-configuration
            home-nix-service-type))

;;; Commentary:
;;;
;;; Home service for Nix package manager configuration.
;;; Manages ~/.config/nix/nix.conf configuration.
;;;
;;; Code:

(define-record-type* <home-nix-configuration>
  home-nix-configuration make-home-nix-configuration
  home-nix-configuration?
  (config-file home-nix-config-file
               (default (mixed-text-file
                         "nix.conf"
                         "experimental-features = nix-command flakes\n"))
               (description "Path to nix.conf configuration file.")))

(define (home-nix-files config)
  "Return alist of Nix configuration files to deploy."
  `((".config/nix/nix.conf" ,(home-nix-config-file config))))

(define home-nix-service-type
  (service-type
   (name 'home-nix)
   (extensions
    (list (service-extension home-files-service-type
                             home-nix-files)))
   (default-value (home-nix-configuration))
   (description
    "Configure Nix package manager for the user.")))
