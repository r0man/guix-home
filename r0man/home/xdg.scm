(define-module (r0man home xdg)
  #:use-module (gnu home services)
  #:use-module (gnu home services xdg)
  #:use-module (guix gexp)
  #:use-module (guix modules))

(define desktop-entry-emacs-client
  (xdg-desktop-entry
   (file "emacs-client")
   (name "Emacs Client")
   (type 'application)
   (config '((exec . "emacsclient --create-frame --alternate-editor=-emacs %u")))))

(define desktop-entries
  (list desktop-entry-emacs-client))

(define xdg-mime-applications-service
  (service home-xdg-mime-applications-service-type
           (home-xdg-mime-applications-configuration
            (default '((inode/directory . emacs-client)
                       (text/plain . emacs-client)
                       (x-scheme-handler/soundklaus emacs-client)))
            (desktop-entries desktop-entries))))

(define-public home-xdg-services
  (list xdg-mime-applications-service))
