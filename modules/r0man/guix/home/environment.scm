(define-module (r0man guix home environment)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (guix records)
  #:export (home-environment-configuration
            home-environment-service-type))

;;; Commentary:
;;;
;;; Home service for environment variables configuration.
;;; Manages shell environment variables.
;;;
;;; Code:

(define-record-type* <home-environment-configuration>
  home-environment-configuration make-home-environment-configuration
  home-environment-configuration?
  (variables home-environment-variables
             (default `(("EDITOR" . "emacsclient")
                        ;; Use NVIDIA GPU (device 1) for ggml/Whisper Vulkan inference
                        ;; instead of Intel Iris Xe (device 0)
                        ("GGML_VULKAN_DEVICE" . "1")
                        ;; Enable NVIDIA PRIME render offload for Vulkan on hybrid graphics
                        ("__NV_PRIME_RENDER_OFFLOAD" . "1")
                        ("__GLX_VENDOR_LIBRARY_NAME" . "nvidia")
                        ("HISTCONTROL" . "ignoredups")
                        ("HISTFILESIZE" . "10000000")
                        ("HISTSIZE" . "100000")
                        ("VISUAL" . "emacsclient")
                        ("_JAVA_AWT_WM_NONREPARENTING" . #t)))
             (description "Alist of environment variables to set.")))

(define (home-environment-vars config)
  "Return alist of environment variables to set."
  (home-environment-variables config))

(define home-environment-service-type
  (service-type
   (name 'home-environment)
   (extensions
    (list (service-extension home-environment-variables-service-type
                             home-environment-vars)))
   (default-value (home-environment-configuration))
   (description
    "Set user environment variables.")))
