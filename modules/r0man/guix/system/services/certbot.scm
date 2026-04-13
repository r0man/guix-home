(define-module (r0man guix system services certbot)
  #:export (certbot-ssl-certificate
            certbot-ssl-certificate-key))

(define (certbot-ssl-certificate domain)
  (format #f "/etc/letsencrypt/live/~a/fullchain.pem" domain))

(define (certbot-ssl-certificate-key domain)
  (format #f "/etc/letsencrypt/live/~a/privkey.pem" domain))
