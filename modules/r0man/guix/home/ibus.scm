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
            (default (list ibus))
            (description "List of IBus-related packages to install.")))

(define (home-ibus-shepherd-services config)
  "Return Shepherd services for IBus."
  ;; Note: The STT engine is started automatically by IBus daemon when
  ;; the engine is activated. A separate shepherd service is not needed
  ;; and would conflict with IBus's engine management.
  ;;
  ;; Environment variables (XDG_DATA_DIRS, GST_PLUGIN_SYSTEM_PATH, etc.)
  ;; are inherited from default-environment-variables, which includes all
  ;; profile search paths set by setup-environment -> $profile/etc/profile.
  ;; With replace-mesa applied to IBus packages, the profile includes the
  ;; NVIDIA Vulkan ICD with absolute store paths.
  ;;
  ;; GGML_VULKAN_DEVICE=1 selects NVIDIA GPU for Whisper inference:
  ;;   GPU 0: Intel Iris Xe (integrated)
  ;;   GPU 1: NVIDIA RTX A1000 (discrete)
  (list (shepherd-service
         (documentation "Run the IBus input method daemon.")
         (provision '(ibus))
         (requirement '(dbus x11-display))
         (modules '((shepherd support)
                    (srfi srfi-1)))
         (start #~(make-forkexec-constructor
                   (list #$(file-append (home-ibus-ibus config)
                                        "/bin/ibus-daemon")
                         "--xim" "--replace" "--verbose")
                   #:environment-variables
                   (cons* "GGML_VULKAN_DEVICE=1"
                          "DISPLAY=:0"
                          (remove (lambda (var)
                                    (or (string-prefix? "GGML_VULKAN_DEVICE=" var)
                                        (string-prefix? "DISPLAY=" var)))
                                  (default-environment-variables)))
                   #:log-file
                   (string-append %user-log-dir "/ibus.log")))
         (stop #~(make-kill-destructor)))))

(define (home-ibus-environment-variables config)
  "Return environment variables for IBus."
  ;; Only IM module configuration is needed here. Search paths like
  ;; XDG_DATA_DIRS and GST_PLUGIN_SYSTEM_PATH come from the profile.
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
