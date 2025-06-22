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
                       (inode/directory . emacsclient.desktop)
                       (text/html . ,browser-mime-type)
                       (text/plain . emacsclient.desktop)
                       (x-scheme-handler/claude . ClaudeDesktop.desktop)
                       (x-scheme-handler/http . ,browser-mime-type)
                       (x-scheme-handler/https . ,browser-mime-type)
                       (x-scheme-handler/slack . com.slack.Slack.desktop)
                       (x-scheme-handler/soundklaus . emacsclient.desktop)
                       (x-scheme-handler/zoommtg . us.zoom.Zoom.desktop)
                       (x-scheme-handler/zsa . ZSTray.desktop)))
            (desktop-entries
             (list (xdg-desktop-entry
                    (file "ClaudeDesktop")
                    (name "Claude Desktop")
                    (type 'application)
                    (config
                     '((exec . "/usr/bin/claude-desktop %u")
                       (MimeType . "x-scheme-handler/claude;")
                       (Terminal . "false"))))
                   (xdg-desktop-entry
                    (file "ZSTray")
                    (name "ZScaler Client Connector")
                    (type 'application)
                    (config
                     '((exec . "/opt/zscaler/scripts/zstray_desktop.sh %U")
                       (MimeType . "x-scheme-handler/zsa;application/x-zstray;")
                       (Terminal . "false")))))))))

(define-public home-xdg-services
  (list xdg-mime-applications-service))
