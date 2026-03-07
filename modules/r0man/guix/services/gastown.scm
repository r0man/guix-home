(define-module (r0man guix services gastown)
  #:use-module (guix records)
  #:export (gastown-crew-configuration
            gastown-crew-configuration?
            gastown-crew-name
            gastown-dolt-configuration
            gastown-dolt-configuration?
            gastown-dolt-port
            gastown-dolt-max-connections
            gastown-dolt-log-level
            gastown-dolt-read-timeout
            gastown-dolt-write-timeout
            gastown-rig-configuration
            gastown-rig-configuration?
            gastown-rig-name
            gastown-rig-git-url
            gastown-rig-prefix
            gastown-rig-crews))

;;; Commentary:
;;;
;;; Shared configuration records for Gas Town services.
;;; Used by both home services and (future) system services.
;;;
;;; Code:

(define-record-type* <gastown-dolt-configuration>
  gastown-dolt-configuration make-gastown-dolt-configuration
  gastown-dolt-configuration?
  (port            gastown-dolt-port            (default 3307))
  (max-connections gastown-dolt-max-connections (default 1000))
  (log-level       gastown-dolt-log-level       (default "warning"))
  (read-timeout    gastown-dolt-read-timeout    (default 300000))
  (write-timeout   gastown-dolt-write-timeout   (default 300000)))

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
