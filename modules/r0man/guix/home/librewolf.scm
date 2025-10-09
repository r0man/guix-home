(define-module (r0man guix home librewolf)
  #:use-module (gnu home services)
  #:use-module (gnu packages librewolf)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:export (home-librewolf-configuration
            home-librewolf-service-type))

;;; Commentary:
;;;
;;; Home service for LibreWolf web browser configuration.
;;; Manages ~/.librewolf/ configuration files and installs LibreWolf.
;;;
;;; Code:

(define-record-type* <home-librewolf-configuration>
  home-librewolf-configuration make-home-librewolf-configuration
  home-librewolf-configuration?
  (overrides-file home-librewolf-overrides-file
                  (default (local-file
                            "files/librewolf/librewolf.overrides.cfg"))
                  (description "Path to librewolf.overrides.cfg file."))
  (packages home-librewolf-packages
            (default (list librewolf))
            (description "List of LibreWolf-related packages to install.")))

(define (home-librewolf-files config)
  "Return alist of LibreWolf configuration files to deploy."
  `((".librewolf/librewolf.overrides.cfg"
     ,(home-librewolf-overrides-file config))))

(define (home-librewolf-profile-packages config)
  "Return list of LibreWolf packages to install."
  (home-librewolf-packages config))

(define home-librewolf-service-type
  (service-type
   (name 'home-librewolf)
   (extensions
    (list (service-extension home-files-service-type
                             home-librewolf-files)
          (service-extension home-profile-service-type
                             home-librewolf-profile-packages)))
   (default-value (home-librewolf-configuration))
   (description
    "Install and configure LibreWolf web browser for the user.")))
