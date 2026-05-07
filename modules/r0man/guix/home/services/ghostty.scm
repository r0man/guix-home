(define-module (r0man guix home services ghostty)
  #:use-module (gnu home services)
  #:use-module (gnu packages bash)
  #:use-module (gnu services configuration)
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

(define-configuration/no-serialization home-ghostty-configuration
  (background-opacity
   (number 0.7)
   "Background opacity (0.0 to 1.0).")
  (font-family
   (string "Hack")
   "Font family name.")
  (font-size
   (number 14.0)
   "Font size in points.")
  (foreground
   (string "#586e75")
   "Foreground text color.")
  (command
   (file-like (file-append bash "/bin/bash --login"))
   "Initial command (shell) to run in ghostty.")
  (packages
   (list-of-packages (list ghostty))
   "List of ghostty-related packages to install."))

(define (home-ghostty-config config)
  "Generate ghostty config file from CONFIG."
  (match-record config <home-ghostty-configuration>
                (background-opacity font-family font-size foreground command)
    (mixed-text-file
     "ghostty-config"
     "background-opacity = " (number->string background-opacity) "\n"
     "font-family = " font-family "\n"
     "font-size = " (number->string font-size) "\n"
     "foreground = " foreground "\n"
     "command = " command "\n")))

(define (home-ghostty-files config)
  "Return alist of ghostty configuration files to deploy."
  `((".config/ghostty/config" ,(home-ghostty-config config))))

(define (home-ghostty-profile-packages config)
  "Return list of ghostty packages to install."
  (home-ghostty-configuration-packages config))

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
