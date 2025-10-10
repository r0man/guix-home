(define-module (r0man guix home gpg)
  #:use-module (gnu home services gnupg)
  #:use-module (gnu home services)
  #:use-module (gnu packages gnupg)
  #:use-module (guix gexp)
  #:export (home-gpg-gtk-configuration
            home-gpg-tty-configuration))

;;; Commentary:
;;;
;;; This module provides pre-configured home-gpg-agent-configuration
;;; instances for common use cases:
;;;
;;; - home-gpg-gtk-configuration: For desktop systems with GTK pinentry
;;; - home-gpg-tty-configuration: For servers with TTY pinentry
;;;
;;; Both configurations use 24-hour cache TTL for GPG and SSH keys.
;;;
;;; Usage in system configs:
;;;   (service home-gpg-agent-service-type home-gpg-gtk-configuration)
;;;   (service home-gpg-agent-service-type home-gpg-tty-configuration)
;;;
;;; Code:

(define home-gpg-default-configuration
  (home-gpg-agent-configuration
    (default-cache-ttl 86400)
    (default-cache-ttl-ssh 86400)
    (max-cache-ttl 86400)
    (max-cache-ttl-ssh 86400)))

(define home-gpg-gtk-configuration
  (home-gpg-agent-configuration
    (inherit home-gpg-default-configuration)
    (pinentry-program (file-append pinentry-gtk2 "/bin/pinentry-gtk-2"))))

(define home-gpg-tty-configuration
  (home-gpg-agent-configuration
    (inherit home-gpg-default-configuration)
    (pinentry-program (file-append pinentry-tty "/bin/pinentry"))))
