(define-module (r0man home shepherd)
  #:use-module (flat packages emacs)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services)
  #:use-module (gnu packages syncthing)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:export (home-shepherd-service))

;; (define emacs-service
;;   (make-service
;;    #:docstring "Emacs daemon"
;;    #:provides '(emacsd)
;;    #:start
;;    (make-system-constructor-with-env
;;     '("emacs" "--no-site-file" "--daemon"))
;;    #:stop
;;    (make-system-destructor
;;     '("emacsclient" "--eval" "(let (kill-emacs-hook) (kill-emacs))"))))

;; (define emacs-service
;;   (shepherd-service
;;    (documentation "Emacs Daemon")
;;    (provision '(emacs))
;;    (start #~(make-forkexec-constructor
;;              (list #$(file-append emacs-native-comp "/bin/emacs")
;;                    "--daemon"
;;                    "--no-site-file")))
;;    (stop  #~(make-kill-destructor))))

(define emacs-service
  (shepherd-service
   (documentation "Emacs Daemon")
   (provision '(emacs-server))
   (start #~(make-forkexec-constructor
             (list #$(file-append emacs-native-comp "/bin/emacs")
                   "--fg-daemon")
             #:log-file (string-append
			 (or (getenv "XDG_LOG_HOME")
			     (format #f "~a/.local/var/log"
				     (getenv "HOME")))
			 "/emacs.log")))
   (stop #~(make-kill-destructor))))

(define my-syncthing-service
  (shepherd-service
   (provision '(syncthing))
   (documentation "Run and control syncthing.")
   (start #~(make-forkexec-constructor
             (list #$(file-append syncthing "/bin/syncthing")
                   "-no-browser")))
   (stop #~(make-kill-destructor))))

(define home-shepherd-service
  (service
   home-shepherd-service-type
   (home-shepherd-configuration
    (services (list emacs-service)))))
