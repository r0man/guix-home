(define-module (r0man guix services gastown)
  #:use-module (guix records)
  #:export (gastown-crew-configuration
            gastown-crew-configuration?
            gastown-crew-name
            gastown-dolt-configuration
            gastown-dolt-configuration?
            gastown-dolt-port
            gastown-dolt-user-name
            gastown-dolt-user-email
            gastown-rig-configuration
            gastown-rig-configuration?
            gastown-rig-name
            gastown-rig-git-url
            gastown-rig-prefix
            gastown-rig-crews
            gastown-town-configuration
            gastown-town-configuration?
            gastown-town-name
            gastown-town-root
            gastown-town-dolt
            gastown-town-rigs))

;;; Commentary:
;;;
;;; Shared configuration records for Gas Town services.
;;; Used by both home services and (future) system services.
;;;
;;; Code:

(define-record-type* <gastown-dolt-configuration>
  gastown-dolt-configuration make-gastown-dolt-configuration
  gastown-dolt-configuration?
  (port       gastown-dolt-port       (default 3307))
  (user-name  gastown-dolt-user-name  (default "Roman Scherer"))
  (user-email gastown-dolt-user-email (default "roman@burningswell.com")))

(define-record-type* <gastown-crew-configuration>
  gastown-crew-configuration make-gastown-crew-configuration
  gastown-crew-configuration?
  (name gastown-crew-name
        (description "Name of the crew workspace (e.g. \"roman\").")))

(define-record-type* <gastown-rig-configuration>
  gastown-rig-configuration make-gastown-rig-configuration
  gastown-rig-configuration?
  (name    gastown-rig-name
           (description "Rig directory name under the Gas Town root (e.g. \"guix_home\")."))
  (git-url gastown-rig-git-url
           (default #f)
           (description "Git remote URL for the rig repository (e.g. \"git@github.com:user/repo\")."))
  (prefix  gastown-rig-prefix
           (default #f)
           (description "Beads issue prefix.  When #f, derived automatically from the rig name."))
  (crews   gastown-rig-crews
           (default '())
           (description "List of <gastown-crew-configuration> records declaring crew workspaces.")))

(define-record-type* <gastown-town-configuration>
  gastown-town-configuration make-gastown-town-configuration
  gastown-town-configuration?
  (name      gastown-town-name
             (description "Town name used for shepherd service naming (e.g. \"gt\")."))
  (town-root gastown-town-root
             (description "Directory relative to $HOME (e.g. \"gt\" → $HOME/gt)."))
  (dolt      gastown-town-dolt
             (default (gastown-dolt-configuration))
             (description "Dolt server configuration (each town needs a unique port)."))
  (rigs      gastown-town-rigs
             (default '())))
