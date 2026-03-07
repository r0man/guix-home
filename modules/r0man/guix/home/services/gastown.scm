(define-module (r0man guix home services gastown)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (r0man guix packages golang-dolthub)
  #:use-module (r0man guix packages task-management)
  #:use-module (r0man guix services gastown)
  #:export (home-gastown-configuration
            home-gastown-service-type))

;;; Commentary:
;;;
;;; Unified home service for Gas Town.
;;;
;;; Manages two shepherd services:
;;;
;;;   gastown-dolt — Dolt SQL server (lifecycle via herd start/stop gastown-dolt).
;;;   gastown      — Gas Town daemon (mayor, witnesses, refineries) via 'gt up'/'gt down'.
;;;                  Requires gastown-dolt.  Runs 'gt up' on start, 'gt down' on stop.
;;;
;;; The home configuration also declares rigs and crew workspaces.  During
;;; 'guix home reconfigure', activation creates rig/crew directory structure
;;; and attempts to register any new rigs with 'gt rig add --adopt'.
;;;
;;; Environment variable GT_TOWN is exported to point at the town root.
;;;
;;; Code:

(define-record-type* <home-gastown-configuration>
  home-gastown-configuration make-home-gastown-configuration
  home-gastown-configuration?
  (town-root       home-gastown-town-root
                   (default "gt")
                   (description "Gas Town directory name relative to $HOME (e.g. \"gt\" for $HOME/gt)."))
  (dolt-config     home-gastown-dolt-config
                   (default (gastown-dolt-configuration))
                   (description "Dolt server configuration."))
  (dolt-user-name  home-gastown-dolt-user-name
                   (default "Roman Scherer")
                   (description "Dolt global user name."))
  (dolt-user-email home-gastown-dolt-user-email
                   (default "roman@burningswell.com")
                   (description "Dolt global user email."))
  (rigs            home-gastown-rigs
                   (default '())
                   (description "List of <gastown-rig-configuration> records declaring managed rigs.")))

(define (home-gastown-shepherd-services config)
  "Return shepherd services for Gas Town: gastown-dolt and gastown."
  (let* ((dolt-bin  (file-append dolt "/bin/dolt"))
         (gt-bin    (file-append gastown-next "/bin/gt"))
         (town-root (home-gastown-town-root config)))
    (list
     (shepherd-service
      (documentation "Run the Gas Town Dolt SQL server.")
      (provision '(gastown-dolt))
      (modules '((shepherd support)))
      (start #~(lambda _
                 (let* ((home (getenv "HOME"))
                        (town (string-append home "/" #$town-root))
                        (work-dir (string-append town "/.dolt-data"))
                        (config-file (string-append home "/.config/gastown/dolt-config.yaml"))
                        (log-file (string-append town "/log/gastown-dolt.log")))
                   (fork+exec-command
                    (list #$dolt-bin "sql-server" "--config" config-file)
                    #:directory work-dir
                    #:log-file log-file))))
      (stop #~(make-kill-destructor SIGTERM)))

     (shepherd-service
      (documentation "Run Gas Town services (daemon, deacon, mayor, witnesses, refineries).")
      (provision '(gastown))
      (requirement '(gastown-dolt))
      (modules '((shepherd support)))
      (start #~(lambda _
                 ;; gt up is idempotent: skips services already running,
                 ;; including gastown-dolt which shepherd manages separately.
                 (let* ((home (getenv "HOME"))
                        (log-file (string-append home "/" #$town-root "/log/gastown.log"))
                        (pid (fork+exec-command (list #$gt-bin "up")
                                               #:log-file log-file)))
                   (waitpid pid)
                   #t)))
      (stop #~(lambda _
                (let* ((home (getenv "HOME"))
                       (log-file (string-append home "/" #$town-root "/log/gastown.log"))
                       (pid (fork+exec-command (list #$gt-bin "down")
                                              #:log-file log-file)))
                  (waitpid pid)
                  #f)))))))

(define (home-gastown-activation config)
  "Return a gexp that creates Gas Town config files at reconfigure time."
  (let* ((dolt-config     (home-gastown-dolt-config config))
         (dolt-port       (gastown-dolt-port dolt-config))
         (max-connections (gastown-dolt-max-connections dolt-config))
         (log-level       (gastown-dolt-log-level dolt-config))
         (read-timeout    (gastown-dolt-read-timeout dolt-config))
         (write-timeout   (gastown-dolt-write-timeout dolt-config))
         (user-name       (home-gastown-dolt-user-name config))
         (user-email      (home-gastown-dolt-user-email config))
         (gt-bin          (file-append gastown-next "/bin/gt"))
         ;; Serialize rig specs as a plain alist for use in the gexp.
         (rig-specs       (map (lambda (rig)
                                 (list (gastown-rig-name rig)
                                       (or (gastown-rig-git-url rig) "")
                                       (or (gastown-rig-prefix rig) "")
                                       (map gastown-crew-name
                                            (gastown-rig-crews rig))))
                               (home-gastown-rigs config))))
    (with-imported-modules '((guix build utils))
      #~(begin
          (use-modules (guix build utils)
                       (ice-9 textual-ports))
          (let* ((home        (getenv "HOME"))
                 (town        (string-append home "/" #$(home-gastown-town-root config)))
                 (dolt-dir    (string-append home "/.dolt"))
                 (config-dir  (string-append home "/.config/gastown"))
                 (log-dir     (string-append town "/log"))
                 (data-dir    (string-append town "/.dolt-data"))
                 (config-yaml (string-append config-dir "/dolt-config.yaml"))
                 (global-json (string-append dolt-dir "/config_global.json")))
            (mkdir-p data-dir)
            (mkdir-p config-dir)
            (mkdir-p log-dir)
            (mkdir-p dolt-dir)
            ;; Generate dolt-config.yaml (always overwritten — runtime $HOME needed for data_dir)
            (call-with-output-file config-yaml
              (lambda (out)
                (display
                 (string-append
                  "log_level: " #$log-level "\n\n"
                  "listener:\n"
                  "  port: " (number->string #$dolt-port) "\n"
                  "  max_connections: " (number->string #$max-connections) "\n"
                  "  read_timeout_millis: " (number->string #$read-timeout) "\n"
                  "  write_timeout_millis: " (number->string #$write-timeout) "\n\n"
                  "data_dir: \"" data-dir "\"\n\n"
                  "behavior:\n"
                  "  auto_gc_behavior:\n"
                  "    enable: true\n"
                  "    archive_level: 1\n")
                 out)))
            ;; Seed config_global.json only if absent — preserves Dolt's auto-generated server_uuid
            (unless (file-exists? global-json)
              (call-with-output-file global-json
                (lambda (out)
                  (display
                   (string-append
                    "{\"user.name\":\"" #$user-name "\","
                    "\"user.email\":\"" #$user-email "\"}\n")
                   out))))
            ;; Set up rig and crew directories, register new rigs with gt.
            (for-each
             (lambda (rig-spec)
               (let* ((rig-name  (car rig-spec))
                      (git-url   (cadr rig-spec))
                      (prefix    (caddr rig-spec))
                      (crews     (cadddr rig-spec))
                      (rig-dir   (string-append town "/" rig-name))
                      (crew-dir  (string-append rig-dir "/crew"))
                      (rigs-json (string-append town "/mayor/rigs.json")))
                 ;; Create basic directory structure for the rig.
                 (mkdir-p crew-dir)
                 ;; Create crew workspace directories.
                 (for-each (lambda (crew-name)
                             (mkdir-p (string-append crew-dir "/" crew-name)))
                           crews)
                 ;; Register rig if not already listed in mayor/rigs.json.
                 ;; Requires gt (gastown-next) and a running Dolt server.
                 ;; Skipped gracefully if Dolt is not available yet.
                 (when (and (file-exists? rigs-json)
                            (not (string-contains
                                  (call-with-input-file rigs-json get-string-all)
                                  (string-append "\"" rig-name "\""))))
                   (let ((args (append (list #$gt-bin "rig" "add" "--adopt" rig-name)
                                       (if (string-null? git-url) '()
                                           (list "--url" git-url))
                                       (if (string-null? prefix) '()
                                           (list "--prefix" prefix)))))
                     (false-if-exception (apply invoke args))))))
             '#$rig-specs))))))

(define (home-gastown-environment-variables config)
  "Return alist of environment variables for Gas Town."
  (let ((town-root (home-gastown-town-root config)))
    `(("GT_TOWN" . ,(string-append "$HOME/" town-root)))))

(define home-gastown-service-type
  (service-type
   (name 'home-gastown)
   (extensions
    (list (service-extension home-shepherd-service-type
                             home-gastown-shepherd-services)
          (service-extension home-activation-service-type
                             home-gastown-activation)
          (service-extension home-environment-variables-service-type
                             home-gastown-environment-variables)))
   (default-value (home-gastown-configuration))
   (description
    "Unified Gas Town home service.  Manages the Dolt SQL server (gastown-dolt)
and the Gas Town daemon (gastown) as user-level shepherd services, and
optionally provisions rig and crew directory structure on activation.")))
