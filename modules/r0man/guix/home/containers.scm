(define-module (r0man guix home containers)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:export (home-containers-service-type))

;;; Commentary:
;;;
;;; Home service for container runtime configuration.
;;; Installs ~/.config/containers/policy.json to allow loading
;;; locally-built OCI images via docker-archive transport.
;;;
;;; Code:

(define (home-containers-xdg-files config)
  `(("containers/policy.json"
     ,(local-file "files/containers/policy.json"))))

(define home-containers-service-type
  (service-type
   (name 'home-containers)
   (extensions
    (list (service-extension home-xdg-configuration-files-service-type
                             home-containers-xdg-files)))
   (default-value #f)
   (description
    "Install container runtime policy configuration.")))
