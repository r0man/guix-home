(define-module (r0man guix services gastown)
  #:use-module (guix records)
  #:export (gastown-dolt-configuration
            gastown-dolt-configuration?
            gastown-dolt-port
            gastown-dolt-max-connections
            gastown-dolt-log-level
            gastown-dolt-read-timeout
            gastown-dolt-write-timeout))

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
