(define-module (r0man guix home kitty)
  #:use-module (gnu home services)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages terminals)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (ice-9 format)
  #:export (home-kitty-configuration
            home-kitty-service-type))

;;; Commentary:
;;;
;;; Home service for Kitty terminal emulator configuration.
;;; Manages ~/.config/kitty/kitty.conf and installs kitty package.
;;;
;;; Code:

(define-record-type* <home-kitty-configuration>
  home-kitty-configuration make-home-kitty-configuration
  home-kitty-configuration?
  (background-opacity home-kitty-background-opacity
                      (default 0.7)
                      (description "Background opacity (0.0 to 1.0)."))
  (font-family home-kitty-font-family
               (default "Hack")
               (description "Font family name."))
  (font-size home-kitty-font-size
             (default 14.0)
             (description "Font size in points."))
  (foreground home-kitty-foreground
              (default "#586e75")
              (description "Foreground text color."))
  (shell home-kitty-shell
         (default (file-append bash "/bin/bash --login"))
         (description "Shell to use in kitty."))
  (packages home-kitty-packages
            (default (list kitty))
            (description "List of kitty-related packages to install.")))

(define (home-kitty-config config)
  "Generate kitty.conf file from CONFIG."
  (mixed-text-file
   "kitty-config"
   "background_opacity " (number->string (home-kitty-background-opacity config)) "\n"
   "font_family " (home-kitty-font-family config) "\n"
   "font_size " (number->string (home-kitty-font-size config)) "\n"
   "foreground " (home-kitty-foreground config) "\n"
   "shell " (home-kitty-shell config) "\n"))

(define (home-kitty-files config)
  "Return alist of kitty configuration files to deploy."
  `((".config/kitty/kitty.conf" ,(home-kitty-config config))))

(define (home-kitty-profile-packages config)
  "Return list of kitty packages to install."
  (home-kitty-packages config))

(define home-kitty-service-type
  (service-type
   (name 'home-kitty)
   (extensions
    (list (service-extension home-files-service-type
                             home-kitty-files)
          (service-extension home-profile-service-type
                             home-kitty-profile-packages)))
   (default-value (home-kitty-configuration))
   (description
    "Install and configure Kitty terminal emulator for the user.")))
