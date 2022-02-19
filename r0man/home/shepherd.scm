(define-module (r0man home shepherd)
  #:use-module (flat packages emacs)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:export (home-shepherd-services))

(define emacs-service
  (shepherd-service
   (documentation "Emacs")
   (provision '(emacs))
   (start #~(make-forkexec-constructor
             (list #$(file-append emacs-native-comp "/bin/emacs")
                   "--fg-daemon")
             #:log-file (string-append
			 (or (getenv "XDG_LOG_HOME")
			     (format #f "~a/.local/var/log"
				     (getenv "HOME")))
			 "/emacs.log")))
   (stop #~(make-kill-destructor))))

(define home-shepherd-services
  (list (service
         home-shepherd-service-type
         (home-shepherd-configuration
          (services (list emacs-service))))))
