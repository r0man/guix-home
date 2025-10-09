(define-module (r0man guix home guile)
  #:use-module (gnu home services)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:export (home-guile-configuration
            home-guile-service-type))

;;; Commentary:
;;;
;;; Home service for GNU Guile Scheme configuration.
;;; Manages ~/.guile configuration and installs Guile packages.
;;;
;;; Code:

(define-record-type* <home-guile-configuration>
  home-guile-configuration make-home-guile-configuration
  home-guile-configuration?
  (config-file home-guile-config-file
               (default (local-file "files/guile"))
               (description "Path to .guile configuration file."))
  (packages home-guile-packages
            (default (list guildhall
                           guile-3.0
                           guile-ares-rs
                           guile-colorized
                           guile-git
                           guile-gnutls
                           guile-json-4
                           guile-lib
                           guile-lzlib
                           guile-readline
                           guile-sqlite3
                           guile-zlib))
            (description "List of Guile-related packages to install.")))

(define (home-guile-files config)
  "Return alist of Guile configuration files to deploy."
  `((".guile" ,(home-guile-config-file config))))

(define (home-guile-profile-packages config)
  "Return list of Guile packages to install."
  (home-guile-packages config))

(define home-guile-service-type
  (service-type
   (name 'home-guile)
   (extensions
    (list (service-extension home-files-service-type
                             home-guile-files)
          (service-extension home-profile-service-type
                             home-guile-profile-packages)))
   (default-value (home-guile-configuration))
   (description
    "Install and configure GNU Guile Scheme for the user.")))
