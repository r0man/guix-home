(define-module (r0man guix home msmtp)
  #:use-module (gnu home services mail)
  #:use-module (gnu home services)
  #:export (home-msmtp-services))

(define %home-msmtp-service
  (service home-msmtp-service-type
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
                 (user "roman.scherer@burningswell.com")))))))))

(define-public home-msmtp-services
  (list %home-msmtp-service))
