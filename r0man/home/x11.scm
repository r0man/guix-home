(define-module (r0man home x11)
  #:use-module (gnu home services)
  #:use-module (gnu packages compton)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages dunst)
  #:use-module (gnu packages image-viewers)
  #:use-module (gnu packages mpd)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:export (home-x11-services))

(define files
  `(("Xresources" ,(local-file "files/Xresources"))
    ("Xresources.bombaclaat" ,(local-file "files/Xresources.bombaclaat"))
    ("config/dunst/dunstrc" ,(local-file "files/dunstrc"))
    ("local/share/wallpapers/stop-customizing-start-working.jpg" ,(local-file "files/wallpapers/stop-customizing-start-working.jpg"))
    ("xbindkeysrc" ,(local-file "files/xbindkeysrc"))
    ("xinitrc" ,(local-file "files/xinitrc"))
    ("xprofile" ,(local-file "files/xprofile"))))

(define packages
  (list dunst
        feh
        light
        mpd
        picom
        unclutter
        xbindkeys
        xdotool
        xsetroot))

(define home-x11-services
  (list (simple-service 'x11-config home-files-service-type files)
        (simple-service 'x11-packages home-profile-service-type packages)))
