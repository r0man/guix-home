(define-module (r0man guix home xdg)
  #:use-module (gnu home services)
  #:use-module (gnu home services xdg)
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

(define-public home-xdg-services
  (list xdg-mime-applications-service))
