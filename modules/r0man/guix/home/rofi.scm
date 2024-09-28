(define-module (r0man guix home rofi)
  #:use-module (gnu home services)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:export (home-rofi-services))

(define %config
  (local-file "files/rofi/config.rasi" "rofi-config.rasi"))

(define %nord-theme
  (local-file "files/rofi/nord.rasi" "rofi-nord-theme.rasi"))

(define files
  `((".config/rofi/config.rasi" ,%config)
    (".config/rofi/nord.rasi" ,%nord-theme)))

(define packages
  (list rofi))

(define home-rofi-services
  (list (simple-service 'rofi-files home-files-service-type files)
        (simple-service 'rofi-profile home-profile-service-type packages)))
