(define-module (r0man guix deploy m1)
  #:use-module (r0man guix system m1)
  #:use-module (gnu machine)
  #:use-module (gnu machine ssh))

(define machines
  (list (machine
         (operating-system m1-operating-system)
         (environment managed-host-environment-type)
         (configuration (machine-ssh-configuration
                         (allow-downgrades? #t)
                         (host-name "localhost")
                         (identity "~/.ssh/id_rsa")
                         (system "aarch64-linux"))))))

machines
