(define-module (r0man guix home xdg)
  #:use-module (gnu home services xdg)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix modules))

(define browser-mime-type
  'librewolf.desktop)

;; See: https://hg.sr.ht/~yoctocell/guixrc/browse/yoctocell/home/xdg.scm?rev=tip

(define xdg-mime-applications-service
  (service home-xdg-mime-applications-service-type
           (home-xdg-mime-applications-configuration
            (default `((application/pdf . org.gnome.Evince.desktop)
                       (application/x-zoom . Zoom.desktop)
                       (inode/directory . emacsclient.desktop)
                       (text/html . ,browser-mime-type)
                       (text/plain . emacsclient.desktop)
                       (x-scheme-handler/http . ,browser-mime-type)
                       (x-scheme-handler/https . ,browser-mime-type)
                       (x-scheme-handler/soundklaus emacsclient.desktop)
                       (x-scheme-handler/zoommtg . Zoom.desktop))))))

(define %xdg-desktop-portals-config
  (mixed-text-file
   "portals.conf"
   "
[preferred]
default=wlr;gtk
"))

(define xdg-files-service
  (simple-service 'xdg-files home-xdg-configuration-files-service-type
                  `(("xdg-desktop-portal/portals.conf" ,%xdg-desktop-portals-config))))

(define-public home-xdg-services
  (list xdg-mime-applications-service xdg-files-service))
