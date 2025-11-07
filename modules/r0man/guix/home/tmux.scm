(define-module (r0man guix home tmux)
  #:use-module (gnu home services)
  #:use-module (gnu packages tmux)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:export (home-tmux-services))

;;; Commentary:
;;;
;;; This module provides tmux configuration for Guix Home.
;;; Configures tmux with 24-bit true color support for kitty terminal.
;;;
;;; Usage in system configs:
;;;   home-tmux-services
;;;
;;; Code:

(define home-tmux-services
  (list
   (simple-service 'home-tmux-packages
                   home-profile-service-type
                   (list tmux))
   (simple-service 'home-tmux-config
                   home-files-service-type
                   `((".tmux.conf" ,(local-file "files/tmux.conf" "tmux.conf"))))))
