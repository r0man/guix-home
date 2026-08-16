(define-module (r0man guix home ssh)
  #:use-module (gnu home services ssh)
  #:use-module (guix gexp)
  #:export (home-openssh-default-configuration))

;;; Commentary:
;;;
;;; This module provides pre-configured home-openssh-configuration
;;; for use with the upstream home-openssh-service-type.
;;;
;;; The configuration includes:
;;; - Automatic SSH key addition to agent
;;; - SSH agent service (use home-ssh-agent-service-type separately)
;;;
;;; Usage in system configs:
;;;   (service home-openssh-service-type
;;;            home-openssh-default-configuration)
;;;   (service home-ssh-agent-service-type)
;;;
;;; Code:

(define %ssh-public-key-roman-burningswell
  (local-file "files/ssh/roman@burningswell.com.pub" "ssh-public-key-roman-burningswell-com"))

(define home-openssh-default-configuration
  (home-openssh-configuration
    (add-keys-to-agent "yes")
    (authorized-keys
     (list %ssh-public-key-roman-burningswell))
    (hosts
     (list (openssh-host
             (name "*")
             (extra-content
              (string-join
               '("ControlMaster auto"
                 "ControlPath ~/.ssh/control-%h-%p-%r"
                 "ControlPersist 10m"
                 ;; Keepalives so a master whose TCP died silently
                 ;; (suspend/resume, Wi-Fi change) self-destructs and
                 ;; frees its control socket instead of wedging every
                 ;; later ssh at mux_client_request_session.
                 "ServerAliveInterval 15"
                 "ServerAliveCountMax 3"
                 "SendEnv COLORTERM")
               "\n")))
           (openssh-host
             (name "localhost")
             (compression? #t)
             (forward-agent? #t)
             (forward-x11-trusted? #t)
             (forward-x11? #t))
           (openssh-host
             (name "www.asahi-guix.org")
             (compression? #t)
             (forward-agent? #t)
             (forward-x11-trusted? #t)
             (forward-x11? #t))))))
