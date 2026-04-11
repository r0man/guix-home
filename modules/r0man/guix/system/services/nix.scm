(define-module (r0man guix system services nix)
  #:use-module (gnu services)
  #:use-module (gnu services nix)
  #:export (%nix-service))

(define %nix-service
  (service nix-service-type))
