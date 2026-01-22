(define-module (r0man guix home container-home)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (r0man guix home systems container)
  #:export (home-container-home-service-type))

;;; Commentary:
;;;
;;; Service that builds container home and creates a profile symlink.
;;;
;;; Uses Guix's gexp-compiler for <home-environment> which automatically
;;; builds the home and returns its store path.
;;;
;;; The result is a symlink: ~/.guix-container-home -> /gnu/store/xxx-home
;;;
;;; Code:

(define home-container-home-service-type
  (service-type
   (name 'home-container-home)
   (extensions
    (list (service-extension home-files-service-type
            (const `((".guix-container-home"
                      ,container-home-environment))))))
   (default-value #f)
   (description "Build container home and create profile symlink.")))
