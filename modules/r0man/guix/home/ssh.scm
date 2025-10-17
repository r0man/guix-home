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

(define %ssh-public-key-roman
  (local-file "files/ssh/roman@precision.pub" "ssh-public-key-roman"))

(define home-openssh-default-configuration
  (home-openssh-configuration
   (add-keys-to-agent "yes")
   (authorized-keys (list %ssh-public-key-roman))
   (hosts
    (list (openssh-host
           (name "*")
           (extra-content
            (string-join
             '("ControlMaster auto"
               "ControlPath /tmp/ssh-%r@%h:%p"
               "ControlPersist 10m")
             "\n")))))))
