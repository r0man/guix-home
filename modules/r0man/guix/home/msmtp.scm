(define-module (r0man guix home msmtp)
  #:use-module (gnu home services mail)
  #:export (home-msmtp-default-configuration))

;;; Commentary:
;;;
;;; This module provides pre-configured home-msmtp-configuration
;;; for use with the upstream home-msmtp-service-type.
;;;
;;; The configuration includes:
;;; - Gmail SMTP server settings (smtp.gmail.com on port 587)
;;; - Default account "roman" for roman@burningswell.com
;;; - TLS with STARTTLS enabled for secure connections
;;; - Password retrieval via 'pass' password manager
;;;
;;; Usage in system configs:
;;;   (service home-msmtp-service-type
;;;            home-msmtp-default-configuration)
;;;
;;; Code:

(define home-msmtp-default-configuration
  (home-msmtp-configuration
   (default-account "roman")
   (accounts
    (list
     (msmtp-account
      (name "roman")
      (configuration
       (msmtp-configuration
        (auth? #t)
        (from "roman@burningswell.com")
        (host "smtp.gmail.com")
        (password-eval "pass Gmail/Burningswell")
        (port 587)
        (tls-starttls? #t)
        (tls? #t)
        (user "roman.scherer@burningswell.com"))))))))
