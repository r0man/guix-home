(define-module (r0man guix home services playerctl)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages music)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:export (home-playerctl-configuration
            home-playerctl-service-type))

;;; Commentary:
;;;
;;; Home service for playerctl.  Installs playerctl and runs playerctld,
;;; which tracks MPRIS player activity so that playerctl commands target
;;; the most recently active player.
;;;
;;; Code:

(define-record-type* <home-playerctl-configuration>
  home-playerctl-configuration make-home-playerctl-configuration
  home-playerctl-configuration?
  (playerctl home-playerctl-package
             (default playerctl)
             (description "The playerctl package to use.")))

(define (home-playerctl-shepherd-services config)
  "Return the playerctld Shepherd service for CONFIG."
  (list (shepherd-service
         (documentation "Track the most recently active MPRIS player.")
         (provision '(playerctld))
         (requirement '(dbus))
         (start #~(make-forkexec-constructor
                   (list #$(file-append (home-playerctl-package config)
                                        "/bin/playerctld"))))
         (stop #~(make-kill-destructor)))))

(define (home-playerctl-profile-packages config)
  "Return list of playerctl packages to install."
  (list (home-playerctl-package config)))

(define home-playerctl-service-type
  (service-type
   (name 'home-playerctl)
   (extensions
    (list (service-extension home-shepherd-service-type
                             home-playerctl-shepherd-services)
          (service-extension home-profile-service-type
                             home-playerctl-profile-packages)))
   (default-value (home-playerctl-configuration))
   (description
    "Install playerctl and run the playerctld MPRIS tracking daemon.")))
