(define-module (r0man home xdg)
  #:use-module (gnu home services)
  #:use-module (gnu home services xdg)
  #:use-module (guix gexp)
  #:use-module (guix modules))

;; See: https://hg.sr.ht/~yoctocell/guixrc/browse/yoctocell/home/xdg.scm?rev=tip

(define xdg-mime-applications-service
  (service home-xdg-mime-applications-service-type
           (home-xdg-mime-applications-configuration
            (default '((application/pdf . org.gnome.Evince.desktop)
                       (application/x-zoom . Zoom.desktop)
                       (inode/directory . emacsclient.desktop)
                       (text/plain . emacsclient.desktop)
                       (x-scheme-handler/http . firefox.desktop)
                       (x-scheme-handler/https . firefox.desktop)
                       (x-scheme-handler/soundklaus emacsclient.desktop)
                       (x-scheme-handler/zoommtg . Zoom.desktop))))))

(define-public home-xdg-services
  (list xdg-mime-applications-service))