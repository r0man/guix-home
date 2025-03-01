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
  #:use-module (gnu services xorg)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (r0man guix system keyboard)
  #:use-module (r0man guix system xorg)
  #:export (home-x11-services
            home-startx-services-precision))

(define files
  `((".Xresources" ,(local-file "files/Xresources"))
    (".Xresources.m1" ,(local-file "files/Xresources.m1"))
    (".Xresources.precision" ,(local-file "files/Xresources.precision"))
    (".Xresources.thinkpad" ,(local-file "files/Xresources.thinkpad"))
    (".config/dunst/dunstrc" ,(local-file "files/dunstrc"))
    (".local/share/wallpapers/stop-customizing-start-working.jpg" ,(local-file "files/wallpapers/stop-customizing-start-working.jpg"))
    (".xbindkeysrc" ,(local-file "files/xbindkeysrc"))
    (".xinitrc" ,(local-file "files/xinitrc"))
    (".xprofile" ,(local-file "files/xprofile"))
    (".xsession" ,(local-file "files/xsession" #:recursive? #t))))

(define packages
  (list arandr
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

(define home-x11-services
  (list (simple-service 'x11-config home-files-service-type files)
        (simple-service 'x11-packages home-profile-service-type packages)
        (service home-x11-service-type)
        (service home-unclutter-service-type)))

(define home-startx-services-precision
  (list (service home-startx-command-service-type
                 (xorg-configuration
                  (keyboard-layout %keyboard-layout)
                  (extra-config (list %xorg-libinput-config))))))
