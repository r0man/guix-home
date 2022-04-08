(define-module (r0man home x11)
  #:use-module (gnu home services)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:export (home-x11-services))

(define files
  `(("Xresources" ,(local-file "files/x11/Xresources"))
    ("Xresources.bombaclaat" ,(local-file "files/x11/Xresources.bombaclaat"))
    ("xbindkeysrc" ,(local-file "files/x11/xbindkeysrc"))
    ("xinitrc" ,(local-file "files/x11/xinitrc"))
    ("xprofile" ,(local-file "files/x11/xprofile"))))

(define packages
  (list xbindkeys))

(define home-x11-services
  (list (simple-service 'x11-config home-files-service-type files)
        (simple-service 'x11-packages home-profile-service-type packages)))
