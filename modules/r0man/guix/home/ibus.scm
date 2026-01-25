(define-module (r0man guix home ibus)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages ibus)
  #:use-module (gnu packages speech)
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
            (default (list ibus ibus-speech-to-text-whisper gst-plugins-good gst-vosk))
            (description "List of IBus-related packages to install.")))

(define (home-ibus-shepherd-services config)
  "Return Shepherd services for IBus."
  (list (shepherd-service
         (documentation "Run the IBus input method daemon.")
         (provision '(ibus))
         (requirement '(dbus))
         (modules '((shepherd support)
                    (srfi srfi-1)
                    (srfi srfi-26)))
         (start #~(lambda _
                    (let* ((home (getenv "HOME"))
                           (profile (string-append home "/.guix-home/profile"))
                           (component-path (string-append profile "/share/ibus/component"))
                           (gst-plugin-path (string-append profile "/lib/gstreamer-1.0")))
                      (fork+exec-command
                       (list #$(file-append (home-ibus-ibus config)
                                            "/bin/ibus-daemon")
                             "--xim"
                             "--replace"
                             "--verbose")
                       #:environment-variables
                       (cons* (string-append "IBUS_COMPONENT_PATH=" component-path)
                              (string-append "GST_PLUGIN_PATH=" gst-plugin-path)
                              (remove (lambda (var)
                                        (or (string-prefix? "IBUS_COMPONENT_PATH=" var)
                                            (string-prefix? "GST_PLUGIN_PATH=" var)))
                                      (default-environment-variables)))
                       #:log-file
                       (string-append %user-log-dir "/ibus.log")))))
         (stop #~(make-kill-destructor)))
        ;; Pre-start the STT engine to work around timing issues with IBus.
        ;; The engine needs to be running before IBus tries to activate it.
        (shepherd-service
         (documentation "Run the IBus Speech-To-Text engine.")
         (provision '(ibus-stt))
         (requirement '(ibus x11-display))
         (modules '((shepherd support)
                    (srfi srfi-1)
                    (srfi srfi-26)))
         (start #~(lambda _
                    (let* ((home (getenv "HOME"))
                           (profile (string-append home "/.guix-home/profile"))
                           (engine-path (string-append profile "/libexec/ibus-engine-stt"))
                           (display (or (getenv "DISPLAY") ":0")))
                      (fork+exec-command
                       (list engine-path "--ibus")
                       #:environment-variables
                       (cons* (string-append "DISPLAY=" display)
                              (string-append "XDG_CURRENT_DESKTOP=STUMPWM")
                              (remove (lambda (var)
                                        (or (string-prefix? "DISPLAY=" var)
                                            (string-prefix? "XDG_CURRENT_DESKTOP=" var)))
                                      (default-environment-variables)))
                       #:log-file
                       (string-append %user-log-dir "/ibus-stt.log")))))
         (stop #~(make-kill-destructor)))))

(define (home-ibus-environment-variables config)
  "Return environment variables for IBus."
  '(("GTK_IM_MODULE" . "ibus")
    ("QT_IM_MODULE" . "ibus")
    ("XMODIFIERS" . "@im=ibus")
    ("GST_PLUGIN_PATH" . "$HOME/.guix-home/profile/lib/gstreamer-1.0")))

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
