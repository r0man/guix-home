(define-module (r0man guix home pm)
  #:use-module (gnu home services pm)
  #:use-module (gnu home services)
  #:use-module (guix records)
  #:export (home-pm-custom-configuration
            make-home-pm-services
            ;; Backward compatibility
            home-pm-services))

;;; Commentary:
;;;
;;; Home service for power management configuration.
;;; Manages batsignal for battery notifications.
;;;
;;; Code:

(define-record-type* <home-pm-custom-configuration>
  home-pm-custom-configuration make-home-pm-custom-configuration
  home-pm-custom-configuration?
  (enable-batsignal? home-pm-enable-batsignal?
                     (default #t)
                     (description "Enable batsignal battery monitor.")))

(define* (make-home-pm-services #:optional
                                (config (home-pm-custom-configuration)))
  "Create list of power management services from configuration."
  (if (home-pm-enable-batsignal? config)
      (list (service home-batsignal-service-type))
      '()))

;; Backward compatibility: keep old service list export
(define home-pm-services
  (make-home-pm-services))
