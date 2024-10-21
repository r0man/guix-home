(define-module (r0man guix home stumpwm)
  #:use-module (gnu home services)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (gnu packages wm)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (r0man guix packages lisp)
  #:use-module (r0man guix packages wm)
  #:export (home-stumpwm-services))

(define-public replace-stumpwm
  (package-input-rewriting/spec
   `(("stumpwm" . ,(const stumpwm-next)))))

(define files
  `((".config/stumpwm/commands.lisp" ,(local-file "files/stumpwm/commands.lisp"))
    (".config/stumpwm/config" ,(local-file "files/stumpwm/config"))
    (".config/stumpwm/polybar.lisp" ,(local-file "files/stumpwm/polybar.lisp"))
    (".config/stumpwm/start" ,(local-file "files/stumpwm/start"))
    (".config/stumpwm/time.lisp" ,(local-file "files/stumpwm/time.lisp"))))

(define packages
  (list sbcl
        sbcl-local-time
        sbcl-slime-swank
        sbcl-slynk
        sbcl-stumpwm-binwarp
        sbcl-stumpwm-battery-portable
        sbcl-stumpwm-cpu
        sbcl-stumpwm-disk
        sbcl-stumpwm-globalwindows
        sbcl-stumpwm-kbd-layouts
        sbcl-stumpwm-mem
        sbcl-stumpwm-net
        sbcl-stumpwm-numpad-layouts
        sbcl-stumpwm-pamixer
        sbcl-stumpwm-pass
        sbcl-stumpwm-screenshot
        sbcl-stumpwm-stumptray
        sbcl-stumpwm-swm-gaps
        sbcl-stumpwm-ttf-fonts
        sbcl-stumpwm-wifi
        sbcl-stumpwm-winner-mode
        stumpish
        stumpwm))

(define packages
  (append
   (list sbcl
         sbcl-local-time
         sbcl-slime-swank
         sbcl-slynk
         stumpwm-next)
   (map replace-stumpwm
        (list sbcl-stumpwm-binwarp
              sbcl-stumpwm-battery-portable
              sbcl-stumpwm-cpu
              sbcl-stumpwm-disk
              sbcl-stumpwm-globalwindows
              sbcl-stumpwm-kbd-layouts
              sbcl-stumpwm-mem
              sbcl-stumpwm-net
              sbcl-stumpwm-numpad-layouts
              sbcl-stumpwm-pamixer
              sbcl-stumpwm-pass
              sbcl-stumpwm-screenshot
              sbcl-stumpwm-stumptray
              sbcl-stumpwm-swm-gaps
              sbcl-stumpwm-ttf-fonts
              sbcl-stumpwm-wifi
              sbcl-stumpwm-winner-mode
              stumpish))))

(define home-stumpwm-services
  (list (simple-service 'stumpwm-config home-files-service-type files)
        (simple-service 'stumpwm-packages home-profile-service-type packages)))
