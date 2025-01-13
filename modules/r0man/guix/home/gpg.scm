(define-module (r0man guix home gpg)
  #:use-module (gnu home services gnupg)
  #:use-module (gnu home services)
  #:use-module (gnu packages gnupg)
  #:use-module (guix gexp)
  #:export (home-gpg-services))

(define %home-gpg-agent-service
  (service home-gpg-agent-service-type
           (home-gpg-agent-configuration
            (default-cache-ttl 86400)
            (default-cache-ttl-ssh 86400)
            (max-cache-ttl 86400)
            (max-cache-ttl-ssh 86400)
            (pinentry-program (file-append pinentry-gtk2 "/bin/pinentry-gtk-2")))))

(define home-gpg-services
  (list %home-gpg-agent-service))
