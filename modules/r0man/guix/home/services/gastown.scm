(define-module (r0man guix home services gastown)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages linux)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (srfi srfi-1)
  #:use-module (r0man guix packages golang-dolthub)
  #:use-module (r0man guix packages task-management)
  #:use-module (r0man guix services gastown)
  #:export (home-gastown-configuration
            home-gastown-service-type
            home-gastown-container-configuration
            home-gastown-container-environment-variables
            home-gastown-container-map-host-runtime-dir?
            home-gastown-container-service-type))

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
;;; Namespace container service for running Gas Town inside a lightweight
;;; Linux namespace (user/mount/pid) via unshare from util-linux.
;;;
;;; Uses a home-environment whose shepherd manages Gas Town services
;;; (gt install/up/down) inside the container.  No Podman dependency.
;;;

(define-record-type* <home-gastown-container-configuration>
  home-gastown-container-configuration make-home-gastown-container-configuration
  home-gastown-container-configuration?
  (home-environment home-gastown-container-home-environment
                    (description "Home environment to run inside the container."))
  (user home-gastown-container-user
        (default "roman")
        (description "Username for host-side paths."))
  (towns home-gastown-container-towns
         (default '())
         (description "List of <gastown-town-configuration> records."))
  (home-mounts home-gastown-container-home-mounts
               (default '(".dolt" ".ssh:ro" ".claude"))
               (description "Paths relative to $HOME to bind-mount.  \
Append ':ro' for read-only."))
  (environment-variables home-gastown-container-environment-variables
                         (default '())
                         (description "List of environment variable names \
to pass through from the host (e.g. '(\"DISPLAY\" \"WAYLAND_DISPLAY\"))."))
  (map-host-runtime-dir? home-gastown-container-map-host-runtime-dir?
                          (default #f)
                          (description "When #t, bind-mount the host \
XDG_RUNTIME_DIR into the container instead of using a fresh tmpfs.")))

(define (gastown-container-script config)
  "Return a computed-file bash script for the namespace container."
  (let* ((he    (home-gastown-container-home-environment config))
         (user  (home-gastown-container-user config))
         (towns (home-gastown-container-towns config))
         (mounts (home-gastown-container-home-mounts config))
         (env-vars (home-gastown-container-environment-variables config))
         (map-runtime? (home-gastown-container-map-host-runtime-dir? config))
         (host-home (string-append "/home/" user))
         (container-home "/tmp/gastown-home"))
    (computed-file "gastown-container-script"
      #~(begin
          (use-modules (ice-9 format))
          (call-with-output-file #$output
            (lambda (port)
              ;; Shebang
              (format port "#!~a\n" #$(file-append bash "/bin/bash"))
              (display "set -euo pipefail\n\n" port)

              ;; Create and mount tmpfs for container home
              (format port "~a -p ~a\n"
                      #$(file-append coreutils "/bin/mkdir")
                      #$container-home)
              (format port "~a -t tmpfs tmpfs ~a\n"
                      #$(file-append util-linux "/bin/mount")
                      #$container-home)

              ;; Create directory structure
              (for-each
               (lambda (dir)
                 (format port "~a -p ~a/~a\n"
                         #$(file-append coreutils "/bin/mkdir")
                         #$container-home dir))
               '(".config/shepherd" ".local/state/shepherd" ".local/run"))

              ;; Create and bind-mount home subdirectories
              (for-each
               (lambda (mount-spec)
                 (let* ((parts (string-split mount-spec #\:))
                        (path (car parts))
                        (ro? (and (not (null? (cdr parts)))
                                  (string=? "ro" (cadr parts)))))
                   (format port "~a -p ~a/~a\n"
                           #$(file-append coreutils "/bin/mkdir")
                           #$container-home path)
                   (format port "~a --bind ~a/~a ~a/~a\n"
                           #$(file-append util-linux "/bin/mount")
                           #$host-home path
                           #$container-home path)
                   (when ro?
                     (format port "~a -o remount,bind,ro ~a/~a\n"
                             #$(file-append util-linux "/bin/mount")
                             #$container-home path))))
               '#$mounts)

              ;; Bind-mount town directories
              (for-each
               (lambda (town-root)
                 (format port "~a -p ~a/~a\n"
                         #$(file-append coreutils "/bin/mkdir")
                         #$container-home town-root)
                 (format port "~a --bind ~a/~a ~a/~a\n"
                         #$(file-append util-linux "/bin/mount")
                         #$host-home town-root
                         #$container-home town-root))
               '#$(map gastown-town-root towns))

              ;; When map-host-runtime-dir? is set, bind-mount the host
              ;; XDG_RUNTIME_DIR (e.g. /run/user/1000) into the container
              ;; at the same path so Wayland/PipeWire sockets are found.
              (when #$map-runtime?
                (display "# Bind-mount host XDG_RUNTIME_DIR\n" port)
                (display "HOST_XDG_RUNTIME_DIR=\"${XDG_RUNTIME_DIR:-}\"\n" port)
                (format port "if [ -n \"${HOST_XDG_RUNTIME_DIR}\" ] && [ -d \"${HOST_XDG_RUNTIME_DIR}\" ]; then\n")
                (format port "  ~a -p \"${HOST_XDG_RUNTIME_DIR}\"\n"
                        #$(file-append coreutils "/bin/mkdir"))
                (format port "  ~a --bind \"${HOST_XDG_RUNTIME_DIR}\" \"${HOST_XDG_RUNTIME_DIR}\"\n"
                        #$(file-append util-linux "/bin/mount"))
                (display "fi\n\n" port))

              ;; Tmpfs for shepherd socket (always isolated from host)
              (format port "~a -t tmpfs -o mode=700 tmpfs ~a/.local/run\n"
                      #$(file-append util-linux "/bin/mount")
                      #$container-home)

              ;; Mount /proc
              (format port "~a -t proc proc /proc\n"
                      #$(file-append util-linux "/bin/mount"))

              ;; Generate and bind-mount /etc/passwd
              (let ((passwd-file (string-append #$container-home "/.passwd")))
                (format port "~a '~a:x:0:0::~a:~a\\n' > ~a\n"
                        #$(file-append coreutils "/bin/printf")
                        #$user #$container-home
                        #$(file-append bash "/bin/bash")
                        passwd-file)
                (format port "~a --bind ~a /etc/passwd\n"
                        #$(file-append util-linux "/bin/mount")
                        passwd-file))

              ;; Mask host nscd socket so glibc reads /etc/passwd directly.
              (format port "~a -t tmpfs tmpfs /var/run/nscd 2>/dev/null || true\n"
                      #$(file-append util-linux "/bin/mount"))

              ;; Export environment
              (format port "\nexport HOME=~a\n" #$container-home)
              (if #$map-runtime?
                  (begin
                    (display "if [ -n \"${HOST_XDG_RUNTIME_DIR:-}\" ]; then\n" port)
                    (display "  export XDG_RUNTIME_DIR=\"${HOST_XDG_RUNTIME_DIR}\"\n" port)
                    (display "else\n" port)
                    (format port "  export XDG_RUNTIME_DIR=~a/.local/run\n"
                            #$container-home)
                    (display "fi\n" port))
                  (format port "export XDG_RUNTIME_DIR=~a/.local/run\n"
                          #$container-home))
              ;; Prevent activate from starting shepherd (it would try
              ;; /var/run/shepherd since UID=0 inside the namespace).
              (display "export GUIX_SYSTEM_IS_RUNNING_HOME_ACTIVATE=1\n" port)

              ;; Pass through configured environment variables from host.
              (for-each
               (lambda (var)
                 (format port "[ -n \"${~a:-}\" ] && export ~a=\"${~a}\"\n"
                         var var var))
               '#$env-vars)
              (newline port)

              ;; Activate home environment (sets up symlinks, env).
              (format port "~a/activate || exit 1\n\n" #$he)

              ;; Start shepherd explicitly with user-mode socket path.
              ;; exec replaces bash so shepherd receives SIGTERM directly
              ;; from --kill-child=SIGTERM for graceful shutdown.
              (let ((socket (string-append #$container-home
                                           "/.local/run/shepherd/socket"))
                    (shepherd (string-append #$he
                                             "/profile/bin/shepherd"))
                    (config (string-append #$container-home
                                           "/.config/shepherd/init.scm"))
                    (log (string-append #$container-home
                                        "/.local/state/shepherd/shepherd.log")))
                (format port "~a -p -m 700 ~a/.local/run/shepherd\n"
                        #$(file-append coreutils "/bin/mkdir")
                        #$container-home)
                (format port "exec ~a --silent --socket=~a --config=~a --logfile=~a\n"
                        shepherd socket config log))))
          (chmod #$output #o755)))))

(define (home-gastown-container-shepherd-service config)
  "Return a shepherd service for the namespace container."
  (let* ((he    (home-gastown-container-home-environment config))
         (user  (home-gastown-container-user config))
         (env-vars (home-gastown-container-environment-variables config))
         (home-dir (string-append "/home/" user))
         (container-home "/tmp/gastown-home")
         (script (gastown-container-script config))
         (nsenter-bin (file-append util-linux "/bin/nsenter"))
         (bash-bin (file-append bash "/bin/bash"))
         ;; Build env export string for nsenter commands.  Each variable
         ;; is conditionally exported only if set in the host environment.
         (env-exports (string-join
                       (map (lambda (var)
                              (string-append
                               "[ -n \"${" var ":-}\" ] && export " var "=\"${" var "}\"; "))
                            env-vars)
                       "")))
    (list
     (shepherd-service
      (documentation "Run Gas Town in a namespace container.")
      (provision '(gastown-container))
      (modules '((shepherd support)
                 (shepherd service)
                 (ice-9 popen)
                 (ice-9 rdelim)
                 (ice-9 format)))
      (respawn-limit #~(cons 3 30))
      (start #~(make-forkexec-constructor
                (list #$(file-append util-linux "/bin/unshare")
                      "--user" "--map-root-user"
                      "--mount" "--pid" "--fork" "--kill-child=SIGTERM"
                      "--" #$script)
                #:log-file (string-append
                            user-homedir
                            "/.local/state/shepherd/gastown-container.log")))
      (stop #~(make-kill-destructor))
      (actions
       (list
        (shepherd-action
         (name 'run)
         (documentation "Run a command inside the container.
Usage: herd run gastown-container COMMAND [ARGS...]
Example: herd run gastown-container gt status")
         (procedure
          #~(lambda (running . args)
              (let* ((pid (process-id running))
                     (child-pid (string-trim-both
                                 (call-with-input-file
                                     (format #f "/proc/~a/task/~a/children"
                                             pid pid)
                                   read-line)))
                     (cmd (string-join args " "))
                     (port (open-input-pipe
                            (format #f "~a -t ~a -U --preserve-credentials -m -p -- ~a -c 'export HOME=~a; export PATH=$HOME/.guix-home/profile/bin:$PATH; ~a~a'"
                                    #$nsenter-bin child-pid #$bash-bin
                                    #$container-home #$env-exports cmd)))
                     (output (read-string port)))
                (close-pipe port)
                (display output)
                #t))))

        (shepherd-action
         (name 'gt-status)
         (documentation "Show gt status for all towns.")
         (procedure
          #~(lambda (running . args)
              (let* ((pid (process-id running))
                     (child-pid (string-trim-both
                                 (call-with-input-file
                                     (format #f "/proc/~a/task/~a/children"
                                             pid pid)
                                   read-line)))
                     (port (open-input-pipe
                            (format #f "~a -t ~a -U --preserve-credentials -m -p -- ~a -c 'export HOME=~a; export PATH=$HOME/.guix-home/profile/bin:$PATH; ~afor t in $HOME/*/mayor; do d=$(dirname $t); cd $d; GT_TOWN=$d gt status; done'"
                                    #$nsenter-bin child-pid #$bash-bin
                                    #$container-home #$env-exports)))
                     (output (read-string port)))
                (close-pipe port)
                (display output)
                #t))))

        (shepherd-action
         (name 'sessions)
         (documentation "List tmux sessions inside the container.")
         (procedure
          #~(lambda (running . args)
              (let* ((pid (process-id running))
                     (child-pid (string-trim-both
                                 (call-with-input-file
                                     (format #f "/proc/~a/task/~a/children"
                                             pid pid)
                                   read-line)))
                     (port (open-input-pipe
                            (format #f "~a -t ~a -U --preserve-credentials -m -p -- ~a -c 'export HOME=~a; export PATH=$HOME/.guix-home/profile/bin:$PATH; ~atmux -L gt ls'"
                                    #$nsenter-bin child-pid #$bash-bin
                                    #$container-home #$env-exports)))
                     (output (read-string port)))
                (close-pipe port)
                (display output)
                #t))))

        (shepherd-action
         (name 'attach)
         (documentation "Print the command to attach to a tmux session.
Usage: herd attach gastown-container [SESSION]
Example: herd attach gastown-container hq-mayor")
         (procedure
          #~(lambda (running . args)
              (let* ((pid (process-id running))
                     (child-pid (string-trim-both
                                 (call-with-input-file
                                     (format #f "/proc/~a/task/~a/children"
                                             pid pid)
                                   read-line)))
                     (session (if (null? args) "" (car args))))
                (format #t "~a -t ~a -U --preserve-credentials -m -p -- ~a -c 'export HOME=~a; export PATH=$HOME/.guix-home/profile/bin:$PATH; ~atmux -L gt attach~a'\n"
                        #$nsenter-bin child-pid #$bash-bin
                        #$container-home #$env-exports
                        (if (string-null? session) ""
                            (string-append " -t " session)))
                #t))))))))))



(define (home-gastown-container-activation config)
  "Ensure host directories exist for bind-mount sources."
  (let* ((towns (home-gastown-container-towns config))
         (mounts (home-gastown-container-home-mounts config)))
    (with-imported-modules '((guix build utils))
      #~(begin
          (use-modules (guix build utils))
          (let ((home (getenv "HOME")))
            ;; Town directories
            (for-each
             (lambda (town-root)
               (mkdir-p (string-append home "/" town-root)))
             '#$(map gastown-town-root towns))
            ;; Home mount sources
            (for-each
             (lambda (mount-spec)
               (let ((path (car (string-split mount-spec #\:))))
                 (mkdir-p (string-append home "/" path))))
             '#$mounts))))))

(define home-gastown-container-service-type
  (service-type
   (name 'home-gastown-container)
   (extensions
    (list (service-extension home-shepherd-service-type
                             home-gastown-container-shepherd-service)
          (service-extension home-activation-service-type
                             home-gastown-container-activation)))
   (description
    "Run a Gas Town home environment inside a Linux namespace container
using unshare.  No Podman dependency.")))
