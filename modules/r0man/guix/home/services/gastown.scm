(define-module (r0man guix home services gastown)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (srfi srfi-1)
  #:use-module (r0man guix packages golang-dolthub)
  #:use-module (r0man guix packages task-management)
  #:use-module (r0man guix services gastown)
  #:export (home-gastown-configuration
            home-gastown-service-type))

;;; Commentary:
;;;
;;; Unified home service for Gas Town with multi-town support.
;;;
;;; For each town, manages one shepherd service:
;;;
;;;   gastown-<name> — Gas Town lifecycle: runs 'gt install --force' (idempotent),
;;;                    registers rigs/crews, then 'gt up'/'gt down'.
;;;                    Gas Town's own daemon manages dolt internally.
;;;                    Stays "running" so 'herd stop' triggers 'gt down'.
;;;
;;; Activation creates the town directory structure (log, .dolt-data, ~/.dolt).
;;;
;;; Code:

(define-record-type* <home-gastown-configuration>
  home-gastown-configuration make-home-gastown-configuration
  home-gastown-configuration?
  (packages home-gastown-packages
            (default (list beads-next dolt gastown-next))
            (description "List of packages to add to the profile."))
  (towns home-gastown-towns
         (default '())
         (description "List of <gastown-town-configuration> records.")))

(define (town-shepherd-services town)
  "Return a shepherd service for TOWN."
  (let* ((name      (gastown-town-name town))
         (town-root (gastown-town-root town))
         (dolt-cfg  (gastown-town-dolt town))
         (port      (gastown-dolt-port dolt-cfg))
         (rigs      (gastown-town-rigs town))
         (gt-bin    (file-append gastown-next "/bin/gt"))
         (svc-main  (symbol-append 'gastown- (string->symbol name)))
         ;; Serialize rig specs as plain alists for use inside gexps.
         (rig-specs (map (lambda (rig)
                           (list (gastown-rig-name rig)
                                 (or (gastown-rig-git-url rig) "")
                                 (or (gastown-rig-prefix rig) "")
                                 (map gastown-crew-name
                                      (gastown-rig-crews rig))))
                         rigs)))
    (list
     (shepherd-service
      (documentation (string-append "Run Gas Town services for town '" name "'."))
      (provision (list svc-main))
      (modules '((shepherd support)
                  (ice-9 textual-ports)))
      (respawn? #f)
      (start #~(lambda _
                 (let* ((town (string-append user-homedir "/" #$town-root))
                        (profile (string-append user-homedir "/.guix-home/profile"))
                        (env (cons* (string-append "GT_TOWN=" town)
                                    (string-append "GT_DOLT_PORT=" (number->string #$port))
                                    (string-append "PATH=" profile "/bin:"
                                                   profile "/sbin")
                                    (string-append "SSL_CERT_DIR=" profile
                                                   "/etc/ssl/certs")
                                    (string-append "SSL_CERT_FILE=" profile
                                                   "/etc/ssl/certs/ca-certificates.crt")
                                    (string-append "GIT_SSL_CAINFO=" profile
                                                   "/etc/ssl/certs/ca-certificates.crt")
                                    (user-environment-variables)))
                        (log-file (string-append town "/log/gastown-" #$name ".log")))
                   ;; Install/upgrade the town (idempotent).
                   (waitpid (fork+exec-command
                             (list #$gt-bin "install" town "--force"
                                   "--no-beads"
                                   "--dolt-port" (number->string #$port))
                             #:directory user-homedir
                             #:log-file log-file
                             #:environment-variables env))
                   ;; Bring up Gas Town services (daemon + dolt).
                   ;; Must run before rig/crew registration since those
                   ;; need dolt running.
                   (waitpid (fork+exec-command (list #$gt-bin "up")
                                               #:directory town
                                               #:log-file log-file
                                               #:environment-variables env))
                   ;; Give dolt time to accept connections.
                   (sleep 3)
                   ;; Register rigs and create crew workspaces.
                   (for-each
                    (lambda (rig-spec)
                      (let* ((rig-name  (car rig-spec))
                             (git-url   (cadr rig-spec))
                             (prefix    (caddr rig-spec))
                             (crews     (cadddr rig-spec))
                             (rig-dir   (string-append town "/" rig-name))
                             (rigs-json (string-append town "/mayor/rigs.json")))
                        ;; Register rig if needed.
                        (cond
                         ;; Dir doesn't exist and git-url is set: gt rig add <name> <url>
                         ((and (not (file-exists? rig-dir))
                               (not (string-null? git-url)))
                          (let ((args (append (list #$gt-bin "rig" "add" rig-name git-url)
                                             (if (string-null? prefix) '()
                                                 (list "--prefix" prefix)))))
                            (waitpid (apply fork+exec-command args
                                           #:directory town
                                           #:log-file log-file
                                           #:environment-variables env
                                           '()))))
                         ;; Dir exists: adopt if not already registered
                         ((file-exists? rig-dir)
                          (let ((already-registered?
                                 (and (file-exists? rigs-json)
                                      (call-with-input-file rigs-json
                                        (lambda (in)
                                          (string-contains (get-string-all in)
                                                           (string-append "\"" rig-name "\"")))))))
                            (unless already-registered?
                              (let ((args (append (list #$gt-bin "rig" "add" "--adopt" rig-name)
                                                  (if (string-null? git-url) '()
                                                      (list "--url" git-url))
                                                  (if (string-null? prefix) '()
                                                      (list "--prefix" prefix)))))
                                (waitpid (apply fork+exec-command args
                                                #:directory town
                                                #:log-file log-file
                                                #:environment-variables env
                                                '())))))))
                        ;; Create crew workspaces.
                        (for-each
                         (lambda (crew-name)
                           (let ((crew-dir (string-append rig-dir "/crew/" crew-name)))
                             (unless (file-exists? crew-dir)
                               (waitpid (fork+exec-command
                                         (list #$gt-bin "crew" "add" crew-name
                                               "--rig" rig-name)
                                         #:directory town
                                         #:log-file log-file
                                         #:environment-variables env)))))
                         crews)))
                    '#$rig-specs)
                   #t)))
      (stop #~(lambda _
                (let* ((town (string-append user-homedir "/" #$town-root))
                       (profile (string-append user-homedir "/.guix-home/profile"))
                       (env (cons* (string-append "GT_TOWN=" town)
                                   (string-append "GT_DOLT_PORT=" (number->string #$port))
                                   (string-append "PATH=" profile "/bin:"
                                                  profile "/sbin")
                                   (user-environment-variables)))
                       (log-file (string-append town "/log/gastown-" #$name ".log")))
                  (waitpid (fork+exec-command (list #$gt-bin "down")
                                              #:directory town
                                              #:log-file log-file
                                              #:environment-variables env))
                  #f)))))))

(define (home-gastown-shepherd-services config)
  "Return shepherd services for all Gas Towns."
  (append-map town-shepherd-services (home-gastown-towns config)))

(define (home-gastown-activation config)
  "Return a gexp that creates directories and seeds Dolt global config."
  (let ((town-activations
         (map (lambda (town)
                (let* ((town-root  (gastown-town-root town))
                       (dolt-cfg   (gastown-town-dolt town))
                       (user-name  (gastown-dolt-user-name dolt-cfg))
                       (user-email (gastown-dolt-user-email dolt-cfg)))
                  #~(let* ((home      (getenv "HOME"))
                           (town      (string-append home "/" #$town-root))
                           (dolt-dir  (string-append home "/.dolt"))
                           (log-dir   (string-append town "/log"))
                           (data-dir  (string-append town "/.dolt-data"))
                           (global-json (string-append dolt-dir "/config_global.json")))
                      (mkdir-p data-dir)
                      (mkdir-p log-dir)
                      (mkdir-p dolt-dir)
                      ;; Seed config_global.json only if absent.
                      (unless (file-exists? global-json)
                        (call-with-output-file global-json
                          (lambda (out)
                            (display
                             (string-append
                              "{\"user.name\":\"" #$user-name "\","
                              "\"user.email\":\"" #$user-email "\"}\n")
                             out)))))))
              (home-gastown-towns config))))
    (with-imported-modules '((guix build utils))
      #~(begin
          (use-modules (guix build utils))
          #$@town-activations))))

(define home-gastown-service-type
  (service-type
   (name 'home-gastown)
   (extensions
    (list (service-extension home-profile-service-type
                             home-gastown-packages)
          (service-extension home-shepherd-service-type
                             home-gastown-shepherd-services)
          (service-extension home-activation-service-type
                             home-gastown-activation)))
   (default-value (home-gastown-configuration))
   (description
    "Multi-town Gas Town home service.  For each town, manages the Gas Town
lifecycle (gt install/up/down) including dolt and the daemon as a user-level
shepherd service.")))
