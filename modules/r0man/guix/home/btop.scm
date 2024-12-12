(define-module (r0man guix home btop)
  #:use-module (gnu home services)
  #:use-module (gnu packages admin)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:export (home-btop-services))

(define config
  (mixed-text-file
   "btop.cfg"
   "color_theme = \"nord\"
    rounded_corners = False
    theme_background = False
    truecolor = True"))

(define files
  `((".config/btop/btop.conf" ,config)))

(define packages
  (list btop))

(define home-btop-services
  (list (simple-service 'btop-config home-files-service-type files)
        (simple-service 'btop-packages home-profile-service-type packages)))
