(define-module (r0man guix home gpg)
  #:use-module (gnu home services gnupg)
  #:use-module (gnu home services)
  #:use-module (gnu packages gnupg)
  #:use-module (guix gexp)
  #:export (home-gpg-gtk-services
            home-gpg-tty-services))

(define %home-gpg-agent-gtk-service
  (service home-gpg-agent-service-type
           (home-gpg-agent-configuration
             (default-cache-ttl 86400)
             (default-cache-ttl-ssh 86400)
             (max-cache-ttl 86400)
             (max-cache-ttl-ssh 86400)
             (pinentry-program (file-append pinentry-gtk2 "/bin/pinentry-gtk-2")))))

(define %home-gpg-agent-tty-service
  (service home-gpg-agent-service-type
           (home-gpg-agent-configuration
             (default-cache-ttl 86400)
             (default-cache-ttl-ssh 86400)
             (max-cache-ttl 86400)
             (max-cache-ttl-ssh 86400)
             (pinentry-program (file-append pinentry-tty "/bin/pinentry")))))

(define home-gpg-gtk-services
  (list %home-gpg-agent-gtk-service))

(define home-gpg-tty-services
  (list %home-gpg-agent-tty-service))
