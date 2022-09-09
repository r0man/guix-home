(define-module (r0man home stumpwm)
  #:use-module (gnu home services)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (gnu packages wm)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (r0man packages lisp)
  #:export (home-stumpwm-services))

(define files
  `((".config/stumpwm/commands.lisp" ,(local-file "files/stumpwm/commands.lisp"))
    (".config/stumpwm/config" ,(local-file "files/stumpwm/config"))
    (".config/stumpwm/polybar.lisp" ,(local-file "files/stumpwm/polybar.lisp"))
    (".config/stumpwm/start" ,(local-file "files/stumpwm/start"))
    (".config/stumpwm/time.lisp" ,(local-file "files/stumpwm/time.lisp"))))

(define packages
  (list cl-stumpwm
        sbcl
        sbcl-local-time
        sbcl-slime-swank
        sbcl-slynk
        sbcl-stumpwm-battery-portable
        sbcl-stumpwm-cpu
        sbcl-stumpwm-disk
        sbcl-stumpwm-globalwindows
        sbcl-stumpwm-kbd-layouts
        sbcl-stumpwm-mem
        sbcl-stumpwm-net
        sbcl-stumpwm-numpad-layouts
        sbcl-stumpwm-pass
        sbcl-stumpwm-screenshot
        sbcl-stumpwm-stumptray
        sbcl-stumpwm-swm-gaps
        sbcl-stumpwm-ttf-fonts
        sbcl-stumpwm-wifi
        sbcl-stumpwm-winner-mode
        stumpish
        stumpwm))

(define home-stumpwm-services
  (list (simple-service 'stumpwm-config home-files-service-type files)
        (simple-service 'stumpwm-packages home-profile-service-type packages)))
