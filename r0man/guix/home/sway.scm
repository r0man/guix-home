(define-module (r0man guix home sway)
  #:use-module (gnu home services)
  #:use-module (gnu packages wm)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (r0man guix packages lisp)
  #:export (home-sway-services))

(define files
  `((".config/sway/config" ,(local-file "files/sway/config"))))

(define packages
  (list sway swaylock swayidle swaybg))

(define home-sway-services
  (list (simple-service 'sway-config home-files-service-type files)
        (simple-service 'sway-packages home-profile-service-type packages)))
