(define-module (r0man home shepherd)
  #:use-module (flat packages emacs)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:export (home-shepherd-service))

(define emacs-daemon-service
  (shepherd-service
   (documentation "Emacs Daemon")
   (provision '(emacs-daemon))
   (start #~(make-forkexec-constructor
             (list #$(file-append emacs-native-comp "/bin/emacs")
                   "--fg-daemon")
             #:log-file (string-append
			 (or (getenv "XDG_LOG_HOME")
			     (format #f "~a/.local/var/log"
				     (getenv "HOME")))
			 "/emacs.log")))
   (stop #~(make-kill-destructor))))

(define home-shepherd-service
  (service
   home-shepherd-service-type
   (home-shepherd-configuration
    (services (list emacs-daemon-service)))))
