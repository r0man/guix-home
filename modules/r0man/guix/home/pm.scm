(define-module (r0man guix home pm)
  #:use-module (gnu home services pm)
  #:export (home-batsignal-default-configuration))

;;; Commentary:
;;;
;;; This module provides pre-configured home-batsignal-configuration
;;; instance for use with the upstream home-batsignal-service-type.
;;;
;;; The configuration uses default batsignal settings for battery
;;; monitoring and notifications.
;;;
;;; Usage in system configs:
;;;   (service home-batsignal-service-type
;;;            home-batsignal-default-configuration)
;;;
;;; Code:

(define home-batsignal-default-configuration
  (home-batsignal-configuration))
