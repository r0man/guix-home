(define-module (r0man guix home services gastown)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (r0man guix packages golang-dolthub)
  #:use-module (r0man guix services gastown)
  #:export (home-gastown-configuration
            home-gastown-service-type))

;;; Commentary:
;;;
;;; Home service for Gas Town's Dolt SQL server.
;;; Manages Dolt as a user-level shepherd daemon with auto-start,
;;; Dolt global configuration, GT_TOWN environment variable, and
;;; Dolt server config file generation.
;;;
;;; The shepherd service owns the Dolt lifecycle — use
;;; 'herd start gastown-dolt' and 'herd stop gastown-dolt'
;;; instead of 'gt dolt start' / 'gt dolt stop'.
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
                   (description "Dolt global user email.")))

(define (home-gastown-shepherd-services config)
  "Return a shepherd service for the Gas Town Dolt SQL server."
  (let* ((dolt-bin  (file-append dolt "/bin/dolt"))
         (town-root (home-gastown-town-root config)))
    (list (shepherd-service
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
           (stop #~(make-kill-destructor SIGTERM))))))

(define (home-gastown-activation config)
  "Return a gexp that creates Gas Town config files at reconfigure time."
  (let* ((dolt-config     (home-gastown-dolt-config config))
         (dolt-port       (gastown-dolt-port dolt-config))
         (max-connections (gastown-dolt-max-connections dolt-config))
         (log-level       (gastown-dolt-log-level dolt-config))
         (read-timeout    (gastown-dolt-read-timeout dolt-config))
         (write-timeout   (gastown-dolt-write-timeout dolt-config))
         (user-name       (home-gastown-dolt-user-name config))
         (user-email      (home-gastown-dolt-user-email config)))
    (with-imported-modules '((guix build utils))
      #~(begin
          (use-modules (guix build utils))
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
                   out)))))))))

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
    "Manage Gas Town's Dolt SQL server as a user-level shepherd daemon.")))
