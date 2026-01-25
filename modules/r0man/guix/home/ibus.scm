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
  ;; Note: The STT engine is started automatically by IBus daemon when
  ;; the engine is activated. A separate shepherd service is not needed
  ;; and would conflict with IBus's engine management.
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
                              ;; Use NVIDIA GPU (device 1) for Whisper Vulkan inference
                              "GGML_VULKAN_DEVICE=1"
                              ;; Enable NVIDIA PRIME render offload for Vulkan on hybrid graphics
                              "__NV_PRIME_RENDER_OFFLOAD=1"
                              "__GLX_VENDOR_LIBRARY_NAME=nvidia"
                              ;; Use profile Vulkan ICDs (with absolute store paths) instead of
                              ;; system ICDs which use relative library paths that fail in Guix
                              (string-append "VK_DRIVER_FILES="
                                             profile "/share/vulkan/icd.d/nvidia_icd.x86_64.json:"
                                             profile "/share/vulkan/icd.d/intel_icd.x86_64.json")
                              (remove (lambda (var)
                                        (or (string-prefix? "IBUS_COMPONENT_PATH=" var)
                                            (string-prefix? "GST_PLUGIN_PATH=" var)
                                            (string-prefix? "GGML_VULKAN_DEVICE=" var)
                                            (string-prefix? "__NV_PRIME_RENDER_OFFLOAD=" var)
                                            (string-prefix? "__GLX_VENDOR_LIBRARY_NAME=" var)
                                            (string-prefix? "VK_DRIVER_FILES=" var)))
                                      (default-environment-variables)))
                       #:log-file
                       (string-append %user-log-dir "/ibus.log")))))
         (stop #~(make-kill-destructor)))))

(define (home-ibus-environment-variables config)
  "Return environment variables for IBus."
  '(("GTK_IM_MODULE" . "ibus")
    ("QT_IM_MODULE" . "ibus")
    ("XMODIFIERS" . "@im=ibus")
    ("GST_PLUGIN_PATH" . "$HOME/.guix-home/profile/lib/gstreamer-1.0")
    ;; Use profile Vulkan ICDs (with absolute store paths) for GPU detection.
    ;; System ICDs use relative library paths that fail in Guix environment.
    ("VK_DRIVER_FILES" . "$HOME/.guix-home/profile/share/vulkan/icd.d/nvidia_icd.x86_64.json:$HOME/.guix-home/profile/share/vulkan/icd.d/intel_icd.x86_64.json")))

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
