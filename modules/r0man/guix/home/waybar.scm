(define-module (r0man guix home waybar)
  #:use-module (gnu home services)
  #:use-module (gnu packages wm)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:export (home-waybar-services))

(define home-waybar-services
  (list (simple-service
         'waybar-xdg-configuration-files
         home-xdg-configuration-files-service-type
         `(("waybar/config" ,(local-file "files/waybar/config"))
           ("waybar/style.css" ,(local-file "files/waybar/style.css"))))
        (simple-service
         'waybar-profile
         home-profile-service-type
         (list waybar))))
