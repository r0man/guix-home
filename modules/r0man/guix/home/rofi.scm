(define-module (r0man guix home rofi)
  #:use-module (gnu home services)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:export (home-rofi-configuration
            home-rofi-service-type))

;;; Commentary:
;;;
;;; Home service for Rofi application launcher configuration.
;;; Manages ~/.config/rofi/ configuration and theme files.
;;;
;;; Code:

(define-record-type* <home-rofi-configuration>
  home-rofi-configuration make-home-rofi-configuration
  home-rofi-configuration?
  (config-file home-rofi-config-file
               (default (local-file "files/rofi/config.rasi"
                                    "rofi-config.rasi"))
               (description "Path to rofi config.rasi file."))
  (theme-file home-rofi-theme-file
              (default (local-file "files/rofi/nord.rasi"
                                   "rofi-nord-theme.rasi"))
              (description "Path to rofi theme file."))
  (packages home-rofi-packages
            (default (list rofi))
            (description "List of rofi-related packages to install.")))

(define (home-rofi-files config)
  "Return alist of rofi configuration files to deploy."
  `((".config/rofi/config.rasi" ,(home-rofi-config-file config))
    (".config/rofi/nord.rasi" ,(home-rofi-theme-file config))))

(define (home-rofi-profile-packages config)
  "Return list of rofi packages to install."
  (home-rofi-packages config))

(define home-rofi-service-type
  (service-type
   (name 'home-rofi)
   (extensions
    (list (service-extension home-files-service-type
                             home-rofi-files)
          (service-extension home-profile-service-type
                             home-rofi-profile-packages)))
   (default-value (home-rofi-configuration))
   (description
    "Install and configure Rofi application launcher for the user.")))
