(define-module (r0man guix home channels)
  #:use-module (r0man guix channels)
  #:use-module (gnu home services guix)
  #:use-module (gnu home services)
  #:use-module (guix ci)
  #:use-module (guix records)
  #:export (home-channels-custom-configuration
            make-home-channels-services
            channels
            ;; Backward compatibility
            home-channels-services))

;;; Commentary:
;;;
;;; Home service for Guix channels configuration.
;;; Manages the list of Guix channels.
;;;
;;; Code:

(define-record-type* <home-channels-custom-configuration>
  home-channels-custom-configuration make-home-channels-custom-configuration
  home-channels-custom-configuration?
  (channels home-channels-list
            (default (list asahi-channel
                           guix-channel
                           nonguix-channel
                           r0man-guix-channel))
            (description "List of Guix channels.")))

(define* (make-home-channels-services #:optional
                                      (config (home-channels-custom-configuration)))
  "Create list of channels services from configuration."
  (list (service home-channels-service-type
                 (home-channels-list config))))

;; Export channels for backward compatibility
(define channels
  (list asahi-channel
        guix-channel
        nonguix-channel
        r0man-guix-channel))

;; Backward compatibility: keep old service list export
(define home-channels-services
  (make-home-channels-services))

channels
