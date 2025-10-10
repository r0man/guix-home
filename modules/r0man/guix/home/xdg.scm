(define-module (r0man guix home xdg)
  #:use-module (gnu home services xdg)
  #:export (home-xdg-mime-applications-default-configuration))

;;; Commentary:
;;;
;;; This module provides pre-configured home-xdg-mime-applications-configuration
;;; for use with the upstream home-xdg-mime-applications-service-type.
;;;
;;; The configuration includes:
;;; - Default MIME type associations (PDF, HTML, directories, etc.)
;;; - Browser set to LibreWolf for web content
;;; - Emacs for text files and directories
;;; - Custom desktop entries for Claude Desktop and ZScaler
;;;
;;; Usage in system configs:
;;;   (service home-xdg-mime-applications-service-type
;;;            home-xdg-mime-applications-default-configuration)
;;;
;;; See: https://hg.sr.ht/~yoctocell/guixrc/browse/yoctocell/home/xdg.scm?rev=tip
;;;
;;; Code:

(define browser-mime-type
  'librewolf.desktop)

(define home-xdg-mime-applications-default-configuration
  (home-xdg-mime-applications-configuration
   (default `((application/pdf . org.gnome.Evince.desktop)
              (application/x-zstray . ZSTray.desktop)
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
            '((exec . "/opt/zscaler/scripts/zstray_desktop.sh %u")
              (MimeType . "x-scheme-handler/zsa;application/x-zstray;")
              (Terminal . "false"))))))))
