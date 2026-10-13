(define-module (r0man guix home services niri)
  #:use-module (gnu home services)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages window-management)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:export (home-niri-configuration
            home-niri-service-type))

;;; Commentary:
;;;
;;; Home service for Niri scrollable-tiling Wayland compositor.
;;; Manages Niri config and Noctalia shell config, and installs required
;;; packages.
;;;
;;; The bar, wallpaper, notifications, OSD, lock screen and idle handling
;;; are provided by Noctalia (https://noctalia.dev), replacing the previous
;;; Waybar + swaybg + swayidle + swaylock stack.  Noctalia is spawned by
;;; Niri (see spawn-at-startup in config-common.kdl) rather than through
;;; Shepherd, so the shell inherits NIRI_SOCKET and the graphical session
;;; environment directly.
;;;
;;; Noctalia's config lives in ~/.config/noctalia/config.toml and is
;;; managed here.  The Settings UI writes runtime overrides to
;;; ~/.local/state/noctalia/settings.toml, which Guix Home leaves alone.
;;;
;;; Code:

(define %noctalia-config
  (local-file "../files/noctalia/config.toml"))

(define %noctalia-setup-marker
  (plain-file "noctalia-setup-complete" ""))

(define-record-type* <home-niri-configuration>
  home-niri-configuration make-home-niri-configuration
  home-niri-configuration?
  (config-file home-niri-config-file
               (default (local-file "../files/niri/config-common.kdl"))
               (description "Niri machine-specific configuration file."))
  (common-config-file home-niri-common-config-file
                      (default (local-file "../files/niri/config-common.kdl"))
                      (description "Niri common configuration file."))
  (noctalia-config home-niri-noctalia-config
                   (default %noctalia-config)
                   (description "Noctalia shell configuration file."))
  (packages home-niri-packages
            (default (list niri
                           noctalia-shell
                           wireplumber
                           wl-clipboard
                           xdg-utils
                           xwayland-satellite))
            (description "List of Niri-related packages to install.")))

(define (home-niri-files config)
  "Return alist of Niri and Noctalia configuration files to deploy."
  `((".config/niri/config.kdl" ,(home-niri-config-file config))
    (".config/niri/config-common.kdl" ,(home-niri-common-config-file config))
    (".config/noctalia/config.toml" ,(home-niri-noctalia-config config))
    ;; Skip Noctalia's first-run setup wizard; config.toml already sets a
    ;; wallpaper and theme.
    (".local/state/noctalia/.setup-complete" ,%noctalia-setup-marker)))

(define (home-niri-profile-packages config)
  "Return list of Niri packages to install."
  (home-niri-packages config))

(define home-niri-service-type
  (service-type
   (name 'home-niri)
   (extensions
    (list (service-extension home-files-service-type
                             home-niri-files)
          (service-extension home-profile-service-type
                             home-niri-profile-packages)))
   (default-value (home-niri-configuration))
   (description
    "Install and configure the Niri scrollable-tiling Wayland compositor
with the Noctalia shell.")))
