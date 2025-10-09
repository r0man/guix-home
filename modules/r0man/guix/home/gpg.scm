(define-module (r0man guix home gpg)
  #:use-module (gnu home services gnupg)
  #:use-module (gnu home services)
  #:use-module (gnu packages gnupg)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:export (home-gpg-custom-configuration
            make-home-gpg-services
            ;; Backward compatibility
            home-gpg-gtk-services
            home-gpg-tty-services))

;;; Commentary:
;;;
;;; Home service for GPG agent configuration.
;;; Manages GPG agent with configurable pinentry program.
;;;
;;; Code:

(define-record-type* <home-gpg-custom-configuration>
  home-gpg-custom-configuration make-home-gpg-custom-configuration
  home-gpg-custom-configuration?
  (pinentry-program home-gpg-pinentry-program
                    (default (file-append pinentry-gtk2
                                          "/bin/pinentry-gtk-2"))
                    (description "Pinentry program for GPG agent."))
  (default-cache-ttl home-gpg-default-cache-ttl
                     (default 86400)
                     (description "Default cache TTL in seconds."))
  (default-cache-ttl-ssh home-gpg-default-cache-ttl-ssh
                         (default 86400)
                         (description "Default SSH cache TTL in seconds."))
  (max-cache-ttl home-gpg-max-cache-ttl
                 (default 86400)
                 (description "Maximum cache TTL in seconds."))
  (max-cache-ttl-ssh home-gpg-max-cache-ttl-ssh
                     (default 86400)
                     (description "Maximum SSH cache TTL in seconds.")))

(define* (make-home-gpg-services #:optional
                                 (config (home-gpg-custom-configuration)))
  "Create list of GPG services from configuration."
  (list (service home-gpg-agent-service-type
                 (home-gpg-agent-configuration
                  (default-cache-ttl (home-gpg-default-cache-ttl config))
                  (default-cache-ttl-ssh (home-gpg-default-cache-ttl-ssh config))
                  (max-cache-ttl (home-gpg-max-cache-ttl config))
                  (max-cache-ttl-ssh (home-gpg-max-cache-ttl-ssh config))
                  (pinentry-program (home-gpg-pinentry-program config))))))

;; Backward compatibility: keep old service list exports
(define home-gpg-gtk-services
  (make-home-gpg-services
   (home-gpg-custom-configuration
    (pinentry-program (file-append pinentry-gtk2 "/bin/pinentry-gtk-2")))))

(define home-gpg-tty-services
  (make-home-gpg-services
   (home-gpg-custom-configuration
    (pinentry-program (file-append pinentry-tty "/bin/pinentry")))))
