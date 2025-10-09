(define-module (r0man guix home common-lisp)
  #:use-module (gnu home services)
  #:use-module (gnu packages lisp)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:export (home-common-lisp-configuration
            home-common-lisp-service-type))

;;; Commentary:
;;;
;;; Home service for Common Lisp (SBCL) configuration.
;;; Manages ~/.sbclrc configuration and installs SBCL.
;;;
;;; Code:

(define-record-type* <home-common-lisp-configuration>
  home-common-lisp-configuration make-home-common-lisp-configuration
  home-common-lisp-configuration?
  (sbclrc-file home-common-lisp-sbclrc-file
               (default (local-file "files/sbclrc"))
               (description "Path to .sbclrc configuration file."))
  (packages home-common-lisp-packages
            (default (list sbcl))
            (description "List of Common Lisp packages to install.")))

(define (home-common-lisp-files config)
  "Return alist of Common Lisp configuration files to deploy."
  `((".sbclrc" ,(home-common-lisp-sbclrc-file config))))

(define (home-common-lisp-profile-packages config)
  "Return list of Common Lisp packages to install."
  (home-common-lisp-packages config))

(define home-common-lisp-service-type
  (service-type
   (name 'home-common-lisp)
   (extensions
    (list (service-extension home-files-service-type
                             home-common-lisp-files)
          (service-extension home-profile-service-type
                             home-common-lisp-profile-packages)))
   (default-value (home-common-lisp-configuration))
   (description
    "Install and configure Common Lisp (SBCL) for the user.")))
