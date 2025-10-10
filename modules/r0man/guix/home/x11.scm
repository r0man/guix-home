(define-module (r0man guix home x11)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services)
  #:use-module (gnu packages compton)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages image-viewers)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages mpd)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:export (home-x11-custom-configuration
            home-x11-custom-service-type))

;;; Commentary:
;;;
;;; This module provides a custom home-x11-custom-service-type for
;;; managing X11 window system configuration files and packages.
;;;
;;; The service manages:
;;; - X11 configuration files (.Xresources, .xinitrc, .xprofile, etc.)
;;; - Desktop notification config (dunst)
;;; - Window manager utilities and X11 tools
;;;
;;; Additionally, use upstream GNU services for X11 features:
;;; - home-x11-service-type: Core X11 configuration
;;; - home-unclutter-service-type: Hide mouse cursor when idle
;;;
;;; Usage in system configs:
;;;   (service home-x11-custom-service-type)
;;;   (service home-x11-service-type)
;;;   (service home-unclutter-service-type)
;;;
;;; Code:

(define-record-type* <home-x11-custom-configuration>
  home-x11-custom-configuration make-home-x11-custom-configuration
  home-x11-custom-configuration?
  (files home-x11-files-list
         (default `((".Xresources" ,(local-file "files/Xresources"))
                    (".Xresources.m1" ,(local-file "files/Xresources.m1"))
                    (".Xresources.precision"
                     ,(local-file "files/Xresources.precision"))
                    (".config/dunst/dunstrc" ,(local-file "files/dunstrc"))
                    (".local/share/wallpapers/stop-customizing-start-working.jpg"
                     ,(local-file
                       "files/wallpapers/stop-customizing-start-working.jpg"))
                    (".xbindkeysrc" ,(local-file "files/xbindkeysrc"))
                    (".xinitrc" ,(local-file "files/xinitrc"))
                    (".xprofile" ,(local-file "files/xprofile"))
                    (".xsession"
                     ,(local-file "files/xsession" #:recursive? #t))
                    ("bin/nerd-dictation-toggle"
                     ,(local-file "files/bin/nerd-dictation-toggle"
                                  #:recursive? #t))))
         (description "Alist of X11 configuration files to deploy."))
  (packages home-x11-packages
            (default (list arandr
                           autorandr
                           dbus
                           dunst
                           feh
                           light
                           mpd
                           pamixer
                           pavucontrol
                           picom
                           setxkbmap
                           xbacklight
                           xbindkeys
                           xclip
                           xdg-utils
                           xdotool
                           xdpyinfo
                           xrandr
                           xrdb
                           xscreensaver
                           xsetroot))
            (description "List of X11-related packages to install.")))

(define (home-x11-files config)
  "Return alist of X11 configuration files to deploy."
  (home-x11-files-list config))

(define (home-x11-profile-packages config)
  "Return list of X11 packages to install."
  (home-x11-packages config))

(define home-x11-custom-service-type
  (service-type
   (name 'home-x11-custom)
   (extensions
    (list (service-extension home-files-service-type
                             home-x11-files)
          (service-extension home-profile-service-type
                             home-x11-profile-packages)))
   (default-value (home-x11-custom-configuration))
   (description
    "Install and configure X11 window system for the user.")))
