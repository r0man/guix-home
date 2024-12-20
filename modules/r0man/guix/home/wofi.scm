(define-module (r0man guix home wofi)
  #:use-module (gnu home services)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:export (home-wofi-services))

;; https://github.com/alxndr13/wofi-nord-theme/blob/master/style.css

(define %wofi-theme
  (mixed-text-file
   "wofi-theme" "

    * {
      font-family: \"Hack\", monospace;
    }

    window {
      background-color: #3B4252;
    }

    #input {
      margin: 5px;
      border-radius: 0px;
      border: none;
      background-color: #3B4252;
      color: white;
    }

    #inner-box {
      background-color: #383C4A;
    }

    #outer-box {
      margin: 2px;
      padding: 10px;
      background-color: #383C4A;
    }

    #scroll {
      margin: 5px;
    }

    #text {
      padding: 4px;
      color: white;
    }

    #entry:nth-child(even){
      background-color: #404552;
    }

    #entry:selected {
      background-color: #4C566A;
    }

    #text:selected {
      background: transparent;
    }"))

(define files
  `((".config/wofi/style.css" ,%wofi-theme)))

(define packages
  (list wofi))

(define home-wofi-services
  (list (simple-service 'wofi-files home-files-service-type files)
        (simple-service 'wofi-profile home-profile-service-type packages)))
