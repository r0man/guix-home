(define-module (r0man guix home ssh)
  #:use-module (gnu home services ssh)
  #:use-module (gnu home services)
  #:use-module (guix gexp))

(define %ssh-public-key-roman
  (local-file "files/ssh/roman@precision.pub" "ssh-public-key-roman"))

(define %home-openssh-service
  (service home-openssh-service-type
           (home-openssh-configuration
            (authorized-keys (list %ssh-public-key-roman)))))

(define-public home-ssh-services
  (list %home-openssh-service))
