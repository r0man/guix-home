(define-module (r0man guix home kitty)
  #:use-module (gnu home services)
  #:use-module (gnu packages terminals)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:export (home-kitty-services))

(define kitty-config
  (mixed-text-file
   "kitty-config"
   "background_opacity 0.7
font_family Hack
font_size 14.0
foreground #586e75
"))

(define home-kitty-services
  (list (simple-service 'kitty-config home-files-service-type
                        `((".config/kitty/kitty.conf" ,kitty-config)))
        (simple-service 'kitty-packages home-profile-service-type
                        (list kitty))))
