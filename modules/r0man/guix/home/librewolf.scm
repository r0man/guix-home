(define-module (r0man guix home librewolf)
  #:use-module (gnu home services)
  #:use-module (gnu packages librewolf)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:export (home-librewolf-services))

(define %librewolf-files
  `((".librewolf/librewolf.overrides.cfg"
     ,(local-file "files/librewolf/librewolf.overrides.cfg"))))

(define %librewolf-profile
  (list librewolf))

(define home-librewolf-services
  (list (simple-service 'librewolf-home-profile
                        home-profile-service-type
                        %librewolf-profile)
        (simple-service 'librewolf-home-files
                        home-files-service-type
                        %librewolf-files)))
