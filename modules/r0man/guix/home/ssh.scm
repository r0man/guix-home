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
;;; - Authorized keys for user roman@precision
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

(define %ssh-public-key-roman-precision
  (local-file "files/ssh/roman@precision.pub" "ssh-public-key-roman-precision"))

(define home-openssh-default-configuration
  (home-openssh-configuration
    (add-keys-to-agent "yes")
    (authorized-keys
     (list %ssh-public-key-roman-burningswell
           %ssh-public-key-roman-precision))
    (hosts
     (list (openssh-host
             (name "*")
             (extra-content
              (string-join
               '("ControlMaster auto"
                 "ControlPath ~/.ssh/control-%h-%p-%r"
                 "ControlPersist 10m"
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
             (forward-x11? #t))
           (openssh-host
             (name "precision")
             (compression? #t)
             (forward-agent? #t)
             (forward-x11-trusted? #t)
             (forward-x11? #t))))))
