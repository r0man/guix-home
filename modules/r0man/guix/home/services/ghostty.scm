(define-module (r0man guix home services ghostty)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:export (home-ghostty-service-type))

;;; Commentary:
;;;
;;; Home service that deploys the Ghostty terminal configuration to
;;; ~/.config/ghostty/config.ghostty.  The ghostty-latest package
;;; itself is installed via packages-desktop in (r0man guix home
;;; packages).
;;;
;;; Code:

(define (home-ghostty-files _)
  `((".config/ghostty/config.ghostty"
     ,(local-file "../files/ghostty/config.ghostty"))))

(define home-ghostty-service-type
  (service-type
   (name 'home-ghostty)
   (extensions
    (list (service-extension home-files-service-type
                             home-ghostty-files)))
   (default-value #f)
   (description
    "Deploy the Ghostty terminal emulator configuration file.")))
