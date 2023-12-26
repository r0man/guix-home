(define-module (r0man guix home wofi)
  #:use-module (gnu home services)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:export (home-wofi-services))

(define %wofi-theme
  (mixed-text-file
   "wofi-theme" "
window {
  margin: 0px;
  border: 1px solid #928374;
  background-color: #002b36;
}

#input {
  margin: 5px;
  border: none;
  color: #839496;
  background-color: #073642;
}

#inner-box {
  margin: 5px;
  border: none;
  background-color: #002b36;
}

#outer-box {
  margin: 5px;
  border: none;
  background-color: #002b36;
}

#scroll {
  margin: 0px;
  border: none;
}

#text {
  margin: 5px;
  border: none;
  color: #839496;
}

#entry:selected {
  background-color: #073642;
}"))

(define files
  `((".config/wofi/style.css" ,%wofi-theme)))

(define packages
  (list wofi))

(define home-wofi-services
  (list (simple-service 'wofi-files home-files-service-type files)
        (simple-service 'wofi-profile home-profile-service-type packages)))
