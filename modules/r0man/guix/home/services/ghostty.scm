(define-module (r0man guix home services ghostty)
  #:use-module (gnu home services)
  #:use-module (gnu packages bash)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (r0man guix packages terminals)
  #:export (home-ghostty-configuration
            home-ghostty-service-type))

;;; Commentary:
;;;
;;; Home service for the Ghostty terminal emulator.  Manages
;;; ~/.config/ghostty/config and installs the ghostty package.
;;;
;;; Code:

(define-record-type* <home-ghostty-configuration>
  home-ghostty-configuration make-home-ghostty-configuration
  home-ghostty-configuration?
  (background-opacity home-ghostty-background-opacity
                      (default 0.7)
                      (description "Background opacity (0.0 to 1.0)."))
  (font-family home-ghostty-font-family
               (default "Hack")
               (description "Font family name."))
  (font-size home-ghostty-font-size
             (default 14.0)
             (description "Font size in points."))
  (foreground home-ghostty-foreground
              (default "#586e75")
              (description "Foreground text color."))
  (command home-ghostty-command
           (default (file-append bash "/bin/bash --login"))
           (description "Initial command (shell) to run in ghostty."))
  (packages home-ghostty-packages
            (default (list ghostty))
            (description "List of ghostty-related packages to install.")))

(define (home-ghostty-config config)
  "Generate ghostty config file from CONFIG."
  (mixed-text-file
   "ghostty-config"
   "background-opacity = " (number->string (home-ghostty-background-opacity config)) "\n"
   "font-family = " (home-ghostty-font-family config) "\n"
   "font-size = " (number->string (home-ghostty-font-size config)) "\n"
   "foreground = " (home-ghostty-foreground config) "\n"
   "command = " (home-ghostty-command config) "\n"))

(define (home-ghostty-files config)
  "Return alist of ghostty configuration files to deploy."
  `((".config/ghostty/config" ,(home-ghostty-config config))))

(define (home-ghostty-profile-packages config)
  "Return list of ghostty packages to install."
  (home-ghostty-packages config))

(define home-ghostty-service-type
  (service-type
   (name 'home-ghostty)
   (extensions
    (list (service-extension home-files-service-type
                             home-ghostty-files)
          (service-extension home-profile-service-type
                             home-ghostty-profile-packages)))
   (default-value (home-ghostty-configuration))
   (description
    "Install and configure the Ghostty terminal emulator for the user.")))
