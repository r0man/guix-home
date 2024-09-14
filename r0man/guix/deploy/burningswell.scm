(define-module (r0man guix deploy burningswell)
  #:use-module (r0man guix system burningswell)
  #:use-module (gnu machine)
  #:use-module (gnu machine ssh))

(define machines
  (list (machine
         (operating-system burningswell-operating-system)
         (environment managed-host-environment-type)
         (configuration (machine-ssh-configuration
                         (allow-downgrades? #t)
                         (host-name "burningswell.com")
                         (identity "~/.ssh/id_rsa")
                         (system "x86_64-linux"))))))

machines
