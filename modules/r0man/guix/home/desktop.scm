(define-module (r0man guix home desktop)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services)
  #:use-module (guix records)
  #:export (home-desktop-custom-configuration
            make-home-dbus-services
            ;; Backward compatibility
            home-dbus-services))

;;; Commentary:
;;;
;;; Home service for desktop services configuration.
;;; Manages D-Bus session bus.
;;;
;;; Code:

(define-record-type* <home-desktop-custom-configuration>
  home-desktop-custom-configuration make-home-desktop-custom-configuration
  home-desktop-custom-configuration?
  (enable-dbus? home-desktop-enable-dbus?
                (default #t)
                (description "Enable D-Bus session bus.")))

(define* (make-home-dbus-services #:optional
                                  (config (home-desktop-custom-configuration)))
  "Create list of desktop services from configuration."
  (if (home-desktop-enable-dbus? config)
      (list (service home-dbus-service-type))
      '()))

;; Backward compatibility: keep old service list export
(define home-dbus-services
  (make-home-dbus-services))
