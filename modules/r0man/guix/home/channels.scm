(define-module (r0man guix home channels)
  #:use-module (r0man guix channels)
  #:use-module (gnu home services guix)
  #:export (home-channels-default-list))

;;; Commentary:
;;;
;;; This module provides pre-configured channel list for use with
;;; the upstream home-channels-service-type.
;;;
;;; The configuration includes channels for:
;;; - guix: Official GNU Guix channel
;;; - nonguix: Non-free software
;;; - asahi: Asahi Linux packages for Apple Silicon
;;; - r0man: Personal channel
;;;
;;; Usage in system configs:
;;;   (service home-channels-service-type home-channels-default-list)
;;;
;;; Code:

(define home-channels-default-list
  (list asahi-channel
        ;; guix-channel
        guix-channel-r0man
        nonguix-channel
        r0man-guix-channel))
