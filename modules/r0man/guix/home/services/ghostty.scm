(define-module (r0man guix home services ghostty)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (r0man guix packages terminals)
  #:export (home-ghostty-service-type
            home-ghostty-terminfo-service-type))

;;; Commentary:
;;;
;;; Home service that deploys the Ghostty terminal configuration to
;;; ~/.config/ghostty/config.ghostty.  The ghostty-latest package
;;; itself is installed via packages-desktop in (r0man guix home
;;; packages).
;;;
;;; home-ghostty-terminfo-service-type deploys only the xterm-ghostty
;;; terminfo entries to ~/.terminfo, for hosts and containers that are
;;; accessed from Ghostty but don't need the terminal itself.
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

(define ghostty-terminfo
  ;; Copy the entries out of ghostty-latest so the result does not
  ;; reference (and keep alive) the whole package.
  (computed-file
   "ghostty-terminfo"
   (with-imported-modules '((guix build utils))
     #~(begin
         (use-modules (guix build utils))
         (copy-recursively #$(file-append ghostty-latest "/share/terminfo")
                           #$output)))))

(define (home-ghostty-terminfo-files _)
  `((".terminfo/g/ghostty" ,(file-append ghostty-terminfo "/g/ghostty"))
    (".terminfo/x/xterm-ghostty"
     ,(file-append ghostty-terminfo "/x/xterm-ghostty"))))

(define home-ghostty-terminfo-service-type
  (service-type
   (name 'home-ghostty-terminfo)
   (extensions
    (list (service-extension home-files-service-type
                             home-ghostty-terminfo-files)))
   (default-value #f)
   (description
    "Deploy the xterm-ghostty terminfo entries to @file{~/.terminfo}.")))
