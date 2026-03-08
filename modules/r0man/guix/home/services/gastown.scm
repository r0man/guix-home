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


;;;
;;; OCI container service for running Gas Town inside rootless Podman.
;;;
;;; Uses a manifest-based image (no full OS) with an entrypoint script
;;; that runs gt install/up and traps SIGTERM for gt down.  The host
;;; shepherd manages the container lifecycle.
;;;

(define-record-type* <home-gastown-oci-configuration>
  home-gastown-oci-configuration make-home-gastown-oci-configuration
  home-gastown-oci-configuration?
  (user home-gastown-oci-user
        (default "roman")
        (description "Username for host-side volume mount paths."))
  (image-value home-gastown-oci-image-value
               (description "Value for the OCI image: a manifest, gexp, \
or file-like object."))
  (towns home-gastown-oci-towns
         (default '())
         (description "List of <gastown-town-configuration> records."))
  (extra-packages home-gastown-oci-extra-packages
                  (default '())
                  (description "Additional packages to include in the image."))
  (home-mounts home-gastown-oci-home-mounts
               (default '(".dolt" ".ssh:ro" ".claude"))
               (description "List of paths relative to $HOME to bind-mount \
into the container.  Append ':ro' for read-only.  Default includes .dolt, \
.ssh (read-only), and .claude."))
  (extra-volumes home-gastown-oci-extra-volumes
                 (default '())
                 (description "Additional volume mounts as strings or pairs.")))

(define (gastown-oci-entrypoint town)
  "Return a shell script for TOWN that installs, starts, and traps
SIGTERM to shut down cleanly.  Uses commands from PATH (set by the
container's profile) rather than absolute store paths."
  (let* ((name      (gastown-town-name town))
         (town-root (gastown-town-root town))
         (dolt-cfg  (gastown-town-dolt town))
         (port      (number->string (gastown-dolt-port dolt-cfg)))
         (rigs      (gastown-town-rigs town))
         (user-name  (gastown-dolt-user-name dolt-cfg))
         (user-email (gastown-dolt-user-email dolt-cfg)))
    (plain-file
     (string-append "gastown-oci-entrypoint-" name ".sh")
     (string-append
      "#!/bin/bash\n"
      "set -euo pipefail\n\n"
      "TOWN_DIR=\"${HOME}/" town-root "\"\n"
      "export GT_TOWN=\"$TOWN_DIR\"\n"
      "export GT_DOLT_PORT=" port "\n\n"
      ;; Create directories.
      "mkdir -p \"$TOWN_DIR/log\" \"$TOWN_DIR/.dolt-data\" \"$HOME/.dolt\"\n\n"
      ;; Seed dolt config.
      "if [ ! -f \"$HOME/.dolt/config_global.json\" ]; then\n"
      "  echo '{\"user.name\":\"" user-name "\","
      "\"user.email\":\"" user-email "\"}' "
      "> \"$HOME/.dolt/config_global.json\"\n"
      "fi\n\n"
      ;; Install and start.
      "echo \"Installing town " name "...\"\n"
      "gt install \"$TOWN_DIR\" --force --no-beads --dolt-port " port "\n\n"
      "cd \"$TOWN_DIR\"\n\n"
      "echo \"Starting town " name "...\"\n"
      "gt up || echo \"Warning: some services failed to start\"\n"
      "sleep 3\n\n"
      ;; Register rigs and crews.
      (apply string-append
             (map (lambda (rig)
                    (let ((rig-name (gastown-rig-name rig))
                          (git-url  (or (gastown-rig-git-url rig) ""))
                          (prefix   (or (gastown-rig-prefix rig) ""))
                          (crews    (map gastown-crew-name
                                        (gastown-rig-crews rig))))
                      (string-append
                       "RIG_DIR=\"$TOWN_DIR/" rig-name "\"\n"
                       (if (string-null? git-url) ""
                           (string-append
                            "if [ ! -d \"$RIG_DIR\" ]; then\n"
                            "  gt rig add " rig-name " " git-url
                            (if (string-null? prefix) ""
                                (string-append " --prefix " prefix))
                            "\n"
                            "elif ! grep -q '\"" rig-name "\"' "
                            "\"$TOWN_DIR/mayor/rigs.json\" 2>/dev/null; then\n"
                            "  gt rig add --adopt " rig-name
                            " --url " git-url
                            (if (string-null? prefix) ""
                                (string-append " --prefix " prefix))
                            "\n"
                            "fi\n"))
                       (apply string-append
                              (map (lambda (crew-name)
                                     (string-append
                                      "if [ ! -d \"$RIG_DIR/crew/" crew-name "\" ]; then\n"
                                      "  gt crew add " crew-name
                                      " --rig " rig-name "\n"
                                      "fi\n"))
                                   crews))
                       "\n")))
                  rigs))
      ;; Trap SIGTERM and block.
      "cleanup() {\n"
      "  echo \"Stopping town " name "...\"\n"
      "  gt down\n"
      "  exit 0\n"
      "}\n"
      "trap cleanup SIGTERM SIGINT\n\n"
      "echo \"Town " name " is running.\"\n"
      "while true; do sleep 3600 & wait $!; done\n"))))

(define (gastown-oci-town-volumes user town)
  "Return volume mount strings for TOWN under USER's home."
  (let* ((home (string-append "/home/" user))
         (root (gastown-town-root town))
         (town-dir (string-append home "/" root)))
    (list (string-append town-dir ":" town-dir))))

(define (home-gastown-oci-extension config)
  "Return an OCI extension with container configurations for each town."
  (let ((user        (home-gastown-oci-user config))
        (image-val   (home-gastown-oci-image-value config))
        (towns       (home-gastown-oci-towns config))
        (extra-pkgs  (home-gastown-oci-extra-packages config))
        (home-mounts (home-gastown-oci-home-mounts config))
        (extra-vols  (home-gastown-oci-extra-volumes config)))
    (let ((image-with-extras
           (if (null? extra-pkgs)
               image-val
               (concatenate-manifests
                (list image-val
                      (packages->manifest extra-pkgs))))))
      (oci-extension
       (containers
        (map (lambda (town)
               (let* ((name (gastown-town-name town))
                      (home (string-append "/home/" user))
                      (log  (string-append home "/.local/state/shepherd/"
                                           "gastown-oci-" name ".log")))
                 (let ((entrypoint (gastown-oci-entrypoint town)))
                   (oci-container-configuration
                    (image (oci-image
                            (repository (string-append "guix/gastown-oci-" name))
                            (tag "latest")
                            (value image-with-extras)
                            (pack-options
                             (list #:symlinks
                                   '(("/bin/sh" -> "bin/bash")
                                     ("/bin/bash" -> "bin/bash"))))))
                    (provision (string-append "gastown-oci-" name))
                    (log-file log)
                    (network "host")
                    (environment
                     (list (cons "HOME" home)))
                    (command (list "/bin/bash" "/entrypoint.sh"))
                    (extra-arguments
                     (list #~(string-append
                              "-v=" #$entrypoint
                              ":/entrypoint.sh:ro")))
                    (volumes (append (gastown-oci-town-volumes user town)
                                     (map (lambda (mount)
                                            (let* ((parts (string-split mount #\:))
                                                   (path  (car parts))
                                                   (opts  (if (null? (cdr parts))
                                                              ""
                                                              (string-append
                                                               ":" (cadr parts)))))
                                              (string-append home "/" path
                                                             ":" home "/" path
                                                             opts)))
                                          home-mounts)
                                     extra-vols))))))
             towns))))))

(define home-gastown-oci-service-type
  (service-type
   (name 'home-gastown-oci)
   (extensions
    (list (service-extension home-oci-service-type
                             home-gastown-oci-extension)))
   (description
    "Run Gas Town services inside OCI containers managed by Podman.")))
