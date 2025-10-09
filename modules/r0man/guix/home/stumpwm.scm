(define-module (r0man guix home stumpwm)
  #:use-module (gnu home services)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (gnu packages wm)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (r0man guix packages lisp)
  #:export (home-stumpwm-configuration
            home-stumpwm-service-type))

;;; Commentary:
;;;
;;; Home service for StumpWM window manager configuration.
;;; Manages ~/.config/stumpwm/ configuration files and installs
;;; StumpWM with Common Lisp packages and contrib modules.
;;;
;;; Code:

(define-record-type* <home-stumpwm-configuration>
  home-stumpwm-configuration make-home-stumpwm-configuration
  home-stumpwm-configuration?
  (commands-file home-stumpwm-commands-file
                 (default (local-file "files/stumpwm/commands.lisp"))
                 (description "Path to commands.lisp file."))
  (config-file home-stumpwm-config-file
               (default (local-file "files/stumpwm/config"))
               (description "Path to main config file."))
  (polybar-file home-stumpwm-polybar-file
                (default (local-file "files/stumpwm/polybar.lisp"))
                (description "Path to polybar.lisp file."))
  (start-file home-stumpwm-start-file
              (default (local-file "files/stumpwm/start"))
              (description "Path to start script file."))
  (time-file home-stumpwm-time-file
             (default (local-file "files/stumpwm/time.lisp"))
             (description "Path to time.lisp file."))
  (packages home-stumpwm-packages
            (default (list sbcl
                           sbcl-local-time
                           sbcl-slime-swank
                           sbcl-slynk
                           sbcl-stumpwm-binwarp
                           sbcl-stumpwm-battery-portable
                           sbcl-stumpwm-cpu
                           sbcl-stumpwm-disk
                           sbcl-stumpwm-globalwindows
                           sbcl-stumpwm-kbd-layouts
                           sbcl-stumpwm-mem
                           sbcl-stumpwm-net
                           sbcl-stumpwm-numpad-layouts
                           sbcl-stumpwm-pamixer
                           sbcl-stumpwm-pass
                           sbcl-stumpwm-screenshot
                           sbcl-stumpwm-stumptray
                           sbcl-stumpwm-swm-gaps
                           sbcl-stumpwm-ttf-fonts
                           sbcl-stumpwm-wifi
                           sbcl-stumpwm-winner-mode
                           stumpish
                           stumpwm))
            (description "List of StumpWM-related packages to install.")))

(define (home-stumpwm-files config)
  "Return alist of StumpWM configuration files to deploy."
  `((".config/stumpwm/commands.lisp"
     ,(home-stumpwm-commands-file config))
    (".config/stumpwm/config"
     ,(home-stumpwm-config-file config))
    (".config/stumpwm/polybar.lisp"
     ,(home-stumpwm-polybar-file config))
    (".config/stumpwm/start"
     ,(home-stumpwm-start-file config))
    (".config/stumpwm/time.lisp"
     ,(home-stumpwm-time-file config))))

(define (home-stumpwm-profile-packages config)
  "Return list of StumpWM packages to install."
  (home-stumpwm-packages config))

(define home-stumpwm-service-type
  (service-type
   (name 'home-stumpwm)
   (extensions
    (list (service-extension home-files-service-type
                             home-stumpwm-files)
          (service-extension home-profile-service-type
                             home-stumpwm-profile-packages)))
   (default-value (home-stumpwm-configuration))
   (description
    "Install and configure StumpWM window manager for the user.")))
