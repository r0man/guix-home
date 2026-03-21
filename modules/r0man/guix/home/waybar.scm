(define-module (r0man guix home waybar)
  #:use-module (gnu home services)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages wm)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix records)
  #:export (home-waybar-configuration
            home-waybar-service-type))

;;; Commentary:
;;;
;;; Home service for Waybar status bar configuration.
;;; Manages ~/.config/waybar/ configuration and style files for
;;; Wayland compositors (Niri, Sway, etc.).
;;;
;;; Code:

(define %waybar-config
  (local-file "files/waybar/config.jsonc"))

(define %waybar-style
  (local-file "files/waybar/style.css"))

(define-record-type* <home-waybar-configuration>
  home-waybar-configuration make-home-waybar-configuration
  home-waybar-configuration?
  (config-file home-waybar-config-file
               (default %waybar-config)
               (description "Path to waybar config.jsonc file."))
  (style-file home-waybar-style-file
              (default %waybar-style)
              (description "Path to waybar style.css file."))
  (packages home-waybar-packages
            (default (list font-awesome pavucontrol waybar))
            (description "List of waybar-related packages to install.")))

(define (home-waybar-files config)
  "Return alist of waybar configuration files to deploy."
  `((".config/waybar/config.jsonc" ,(home-waybar-config-file config))
    (".config/waybar/style.css" ,(home-waybar-style-file config))))

(define (home-waybar-profile-packages config)
  "Return list of waybar packages to install."
  (home-waybar-packages config))

(define home-waybar-service-type
  (service-type
   (name 'home-waybar)
   (extensions
    (list (service-extension home-files-service-type
                             home-waybar-files)
          (service-extension home-profile-service-type
                             home-waybar-profile-packages)))
   (default-value (home-waybar-configuration))
   (description
    "Install and configure Waybar status bar for Wayland compositors.")))
