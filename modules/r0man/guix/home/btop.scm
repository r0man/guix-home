(define-module (r0man guix home btop)
  #:use-module (gnu home services)
  #:use-module (gnu packages admin)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (ice-9 format)
  #:export (home-btop-configuration
            home-btop-service-type))

;;; Commentary:
;;;
;;; Home service for btop system monitor configuration.
;;; Manages ~/.config/btop/btop.conf and installs btop package.
;;;
;;; Code:

(define-record-type* <home-btop-configuration>
  home-btop-configuration make-home-btop-configuration
  home-btop-configuration?
  (color-theme home-btop-color-theme
               (default "nord")
               (description "Color theme name."))
  (rounded-corners? home-btop-rounded-corners?
                    (default #f)
                    (description "Enable rounded corners."))
  (theme-background? home-btop-theme-background?
                     (default #f)
                     (description "Use theme background."))
  (truecolor? home-btop-truecolor?
              (default #t)
              (description "Enable truecolor support."))
  (packages home-btop-packages
            (default (list btop))
            (description "List of btop-related packages to install.")))

(define (home-btop-config config)
  "Generate btop.conf file from CONFIG."
  (mixed-text-file
   "btop.cfg"
   (format #f "color_theme = \"~a\"
    rounded_corners = ~a
    theme_background = ~a
    truecolor = ~a"
           (home-btop-color-theme config)
           (if (home-btop-rounded-corners? config) "True" "False")
           (if (home-btop-theme-background? config) "True" "False")
           (if (home-btop-truecolor? config) "True" "False"))))

(define (home-btop-files config)
  "Return alist of btop configuration files to deploy."
  `((".config/btop/btop.conf" ,(home-btop-config config))))

(define (home-btop-profile-packages config)
  "Return list of btop packages to install."
  (home-btop-packages config))

(define home-btop-service-type
  (service-type
   (name 'home-btop)
   (extensions
    (list (service-extension home-files-service-type
                             home-btop-files)
          (service-extension home-profile-service-type
                             home-btop-profile-packages)))
   (default-value (home-btop-configuration))
   (description
    "Install and configure btop system monitor for the user.")))
