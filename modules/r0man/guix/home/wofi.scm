(define-module (r0man guix home wofi)
  #:use-module (gnu home services)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:export (home-wofi-configuration
            home-wofi-service-type))

;;; Commentary:
;;;
;;; Home service for Wofi application launcher (Wayland-native rofi).
;;; Manages ~/.config/wofi/style.css and installs wofi package.
;;; Theme based on: https://github.com/alxndr13/wofi-nord-theme
;;;
;;; Code:

(define-record-type* <home-wofi-configuration>
  home-wofi-configuration make-home-wofi-configuration
  home-wofi-configuration?
  (theme-file home-wofi-theme-file
              (default (mixed-text-file
                        "wofi-theme" "

    * {
      font-family: \"Hack\", monospace;
    }

    window {
      background-color: #4c566a;
      border-color: #3B4252;
      border-radius: 0px;
      border: 1em;
    }

    #input {
      margin: 5px;
      border-radius: 0px;
      border: none;
      background-color: #3B4252;
      color: white;
    }

    #inner-box {
      background-color: #383C4A;
    }

    #outer-box {
      margin: 2px;
      padding: 10px;
      background-color: #383C4A;
    }

    #scroll {
      margin: 5px;
    }

    #text {
      padding: 4px;
      color: white;
    }

    #entry:nth-child(even){
      background-color: #404552;
    }

    #entry:selected {
      background-color: #4C566A;
    }

    #text:selected {
      background: transparent;
    }"))
              (description "Path to wofi theme CSS file."))
  (packages home-wofi-packages
            (default (list wofi))
            (description "List of wofi-related packages to install.")))

(define (home-wofi-files config)
  "Return alist of wofi configuration files to deploy."
  `((".config/wofi/style.css" ,(home-wofi-theme-file config))))

(define (home-wofi-profile-packages config)
  "Return list of wofi packages to install."
  (home-wofi-packages config))

(define home-wofi-service-type
  (service-type
   (name 'home-wofi)
   (extensions
    (list (service-extension home-files-service-type
                             home-wofi-files)
          (service-extension home-profile-service-type
                             home-wofi-profile-packages)))
   (default-value (home-wofi-configuration))
   (description
    "Install and configure Wofi application launcher for Wayland.")))
