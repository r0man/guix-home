(define-module (r0man guix system services console-font)
  #:use-module (gnu packages fonts)
  #:use-module (guix gexp)
  #:export (console-font-service-config))

(define (console-font-service-config config)
  (map (lambda (tty)
         (cons tty (file-append font-terminus "/share/consolefonts/ter-132n")))
       '("tty1" "tty2" "tty3" "tty4" "tty5" "tty6")))
