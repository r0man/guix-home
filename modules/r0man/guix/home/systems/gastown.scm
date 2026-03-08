(define-module (r0man guix home systems gastown)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages version-control)
  #:use-module (gnu services)
  #:use-module (r0man guix home services gastown)
  #:use-module (r0man guix services gastown))

;;; Commentary:
;;;
;;; Gas Town home environment for running inside a namespace container.
;;; Uses the home environment's shepherd to manage Gas Town services,
;;; eliminating the need for a custom entrypoint script.
;;;
;;; Code:

(define-public (make-gastown-home-environment towns extra-packages)
  "Create a home environment for Gas Town with TOWNS and EXTRA-PACKAGES.
TOWNS is a list of <gastown-town-configuration> records.
EXTRA-PACKAGES is a list of additional packages to include."
  (home-environment
   (packages (append (list bash coreutils git guix nss-certs) extra-packages))
   (services (list (service home-gastown-service-type
                            (home-gastown-configuration
                             (towns towns)))))))
