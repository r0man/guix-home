(define-module (asahi guix home services sound)
  #:use-module (asahi guix packages crates-io)
  #:use-module (asahi guix packages linux)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services xdg)
  #:use-module (gnu home services)
  #:use-module (gnu packages music)
  #:use-module (gnu services configuration)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:export (home-pulseaudio-rtp-sink-service-type
            home-pulseaudio-rtp-source-service-type
            %pulseaudio-rtp-multicast-address

            home-pipewire-configuration
            home-pipewire-service-type))


;;;
;;; PipeWire support.
;;;

(define-configuration/no-serialization home-pipewire-configuration
  (alsa-ucm-conf
   (file-like asahi-alsa-ucm-conf)
   "The Alsa Use Case Manager to use.")
  (bankstown
   (file-like rust-bankstown)
   "The Bass enhancer package to use.")
  (lsp-plugins
   (file-like lsp-plugins)
   "The Lsp Plugins package to use.")
  (pipewire
   (file-like asahi-pipewire)
   "The PipeWire package to use.")
  (wireplumber
   (file-like asahi-wireplumber)
   "The WirePlumber package to use.")
  (enable-pulseaudio?
   (boolean #t)
   "When true, enable PipeWire's PulseAudio emulation support, allowing
PulseAudio clients to use PipeWire transparently."))

(define (home-pipewire-shepherd-service config)
  (shepherd-service
   (documentation "PipeWire media processing.")
   (provision '(pipewire))
   (requirement '(dbus))
   (start #~(make-forkexec-constructor
             (list #$(file-append
                      (home-pipewire-configuration-pipewire config)
                      "/bin/pipewire"))
             #:environment-variables
             (list "DISABLE_RTKIT=1"
                   (string-append
                    "ALSA_CONFIG_UCM2="
                    #$(home-pipewire-configuration-alsa-ucm-conf config)
                    "/share/alsa/ucm2")
                   (string-append
                    "LV2_PATH="
                    #$(home-pipewire-configuration-bankstown config)
                    "/lib/lv2"
                    ":"
                    #$(home-pipewire-configuration-lsp-plugins config)
                    "/lib/lv2")
                   (string-append
                    "PIPEWIRE_MODULE_DIR="
                    #$(home-pipewire-configuration-pipewire config)
                    "/lib/pipewire-0.3")
                   (string-append
                    "XDG_RUNTIME_DIR="
                    (or (getenv "XDG_RUNTIME_DIR")
                        (format #f "/run/user/~a" (getuid)))))))
   (stop #~(make-kill-destructor))))

(define (home-pipewire-pulseaudio-shepherd-service config)
  (shepherd-service
   (documentation "Drop-in PulseAudio replacement service for PipeWire.")
   (provision '(pipewire-pulseaudio))
   (requirement '(pipewire))
   (start #~(make-forkexec-constructor
             (list #$(file-append
                      (home-pipewire-configuration-pipewire config)
                      "/bin/pipewire-pulse"))
             #:environment-variables
             (list "DISABLE_RTKIT=1"
                   (string-append
                    "ALSA_CONFIG_UCM2="
                    #$(home-pipewire-configuration-alsa-ucm-conf config)
                    "/share/alsa/ucm2")
                   (string-append
                    "LV2_PATH="
                    #$(home-pipewire-configuration-bankstown config)
                    "/lib/lv2"
                    ":"
                    #$(home-pipewire-configuration-lsp-plugins config)
                    "/lib/lv2")
                   (string-append
                    "PIPEWIRE_MODULE_DIR="
                    #$(home-pipewire-configuration-pipewire config)
                    "/lib/pipewire-0.3")
                   (string-append
                    "XDG_RUNTIME_DIR="
                    (or (getenv "XDG_RUNTIME_DIR")
                        (format #f "/run/user/~a" (getuid)))))))
   (stop #~(make-kill-destructor))))

(define (home-wireplumber-shepherd-service config)
  (shepherd-service
   (documentation "WirePlumber session management for PipeWire.")
   (provision '(wireplumber))
   (requirement '(pipewire))
   (start #~(make-forkexec-constructor
             (list #$(file-append
                      (home-pipewire-configuration-wireplumber config)
                      "/bin/wireplumber"))
             #:environment-variables
             (list "DISABLE_RTKIT=1"
                   (string-append
                    "ALSA_CONFIG_UCM2="
                    #$(home-pipewire-configuration-alsa-ucm-conf config)
                    "/share/alsa/ucm2")
                   (string-append
                    "LV2_PATH="
                    #$(home-pipewire-configuration-bankstown config)
                    "/lib/lv2"
                    ":"
                    #$(home-pipewire-configuration-lsp-plugins config)
                    "/lib/lv2")
                   (string-append
                    "XDG_RUNTIME_DIR="
                    (or (getenv "XDG_RUNTIME_DIR")
                        (format #f "/run/user/~a" (getuid)))))))
   (stop #~(make-kill-destructor))))

(define (home-pipewire-shepherd-services config)
  (cons* (home-pipewire-shepherd-service config)
         (home-wireplumber-shepherd-service config)
         (if (home-pipewire-configuration-enable-pulseaudio? config)
             (list (home-pipewire-pulseaudio-shepherd-service config))
             '())))

(define (home-pipewire-asoundrc config)
  (match-record config <home-pipewire-configuration>
                (pipewire)
    (mixed-text-file
     "asoundrc"
     "<" pipewire "/share/alsa/alsa.conf.d/50-pipewire.conf>\n"
     "<" pipewire "/share/alsa/alsa.conf.d/99-pipewire-default.conf>\n"
     "pcm_type.pipewire {\n"
     "  lib \"" pipewire "/lib/alsa-lib/libasound_module_pcm_pipewire.so\"\n"
     "}\n"
     "ctl_type.pipewire {\n"
     "  lib \"" pipewire "/lib/alsa-lib/libasound_module_ctl_pipewire.so\"\n"
     "}\n")))

(define (home-pipewire-conf config)
  (file-append (home-pipewire-configuration-pipewire config) "/share/pipewire/pipewire.conf"))

(define (home-wireplumber-conf config)
  (file-append (home-pipewire-configuration-wireplumber config) "/share/wireplumber/wireplumber.conf"))

(define home-pipewire-disable-pulseaudio-auto-start
  (plain-file "client.conf" "autospawn = no"))

(define (home-pipewire-xdg-configuration config)
  (cons* `("alsa/asoundrc" ,(home-pipewire-asoundrc config))
         ;; `("pipewire/pipewire.conf" ,(home-pipewire-conf config))
         ;; `("pipewire/wireplumber.conf" ,(home-wireplumber-conf config))
         (if (home-pipewire-configuration-enable-pulseaudio? config)
             `(("pulse/client.conf"
                ,home-pipewire-disable-pulseaudio-auto-start))
             '())))

(define (home-pipewire-profile-entries config)
  (list (home-pipewire-configuration-lsp-plugins config)
        (home-pipewire-configuration-pipewire config)
        (home-pipewire-configuration-wireplumber config)))

(define home-pipewire-service-type
  (service-type
   (name 'pipewire)
   (extensions
    (list (service-extension home-profile-service-type
                             home-pipewire-profile-entries)
          (service-extension home-shepherd-service-type
                             home-pipewire-shepherd-services)
          (service-extension home-xdg-configuration-files-service-type
                             home-pipewire-xdg-configuration)))
   (description
    "Start essential PipeWire services.")
   (default-value (home-pipewire-configuration))))
