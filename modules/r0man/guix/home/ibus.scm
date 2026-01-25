(define-module (r0man guix home ibus)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services)
  #:use-module (gnu packages ibus)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:export (home-ibus-configuration
            home-ibus-service-type))

;;; Commentary:
;;;
;;; Home service for IBus input method framework.
;;; Manages IBus daemon, environment variables, and packages.
;;;
;;; Code:

(define-record-type* <home-ibus-configuration>
  home-ibus-configuration make-home-ibus-configuration
  home-ibus-configuration?
  (ibus home-ibus-ibus
        (default ibus)
        (description "The IBus package to use."))
  (packages home-ibus-packages
            (default (list ibus ibus-speech-to-text))
            (description "List of IBus-related packages to install.")))

(define (home-ibus-shepherd-services config)
  "Return Shepherd services for IBus."
  (list (shepherd-service
         (documentation "Run the IBus input method daemon.")
         (provision '(ibus))
         (requirement '(dbus))
         (modules '((shepherd support)))
         (start #~(make-forkexec-constructor
                   (list #$(file-append (home-ibus-ibus config)
                                        "/bin/ibus-daemon")
                         "--xim"
                         "--daemonize"
                         "--replace")
                   #:log-file
                   (string-append %user-log-dir "/ibus.log")))
         (stop #~(make-kill-destructor)))))

(define (home-ibus-environment-variables config)
  "Return environment variables for IBus."
  '(("GTK_IM_MODULE" . "ibus")
    ("QT_IM_MODULE" . "ibus")
    ("XMODIFIERS" . "@im=ibus")))

(define (home-ibus-profile-packages config)
  "Return list of IBus packages to install."
  (home-ibus-packages config))

(define home-ibus-service-type
  (service-type
   (name 'home-ibus)
   (extensions
    (list (service-extension home-shepherd-service-type
                             home-ibus-shepherd-services)
          (service-extension home-environment-variables-service-type
                             home-ibus-environment-variables)
          (service-extension home-profile-service-type
                             home-ibus-profile-packages)))
   (default-value (home-ibus-configuration))
   (description
    "Run the IBus input method framework daemon and configure
environment variables for input method support.")))
