(define-module (r0man guix home nix)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:export (home-nix-services))

(define files
  `((".config/nix/nix.conf"
     ,(mixed-text-file
       "nix.conf"
       "experimental-features = nix-command flakes\n"))))

(define home-nix-services
  (list (simple-service 'nix-config home-files-service-type files)))
