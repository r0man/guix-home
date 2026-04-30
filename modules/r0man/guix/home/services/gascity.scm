(define-module (r0man guix home services gascity)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu packages base)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lsof)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages tmux)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages web)
  #:use-module (r0man guix packages golang-dolthub)
  #:use-module (r0man guix packages task-management)
  #:export (gascity-rig-configuration
            gascity-rig-configuration?
            gascity-rig-path
            gascity-rig-git-url
            gascity-city-configuration
            gascity-city-configuration?
            gascity-city-path
            gascity-city-name
            gascity-city-rigs
            home-gascity-configuration
            home-gascity-configuration?
            home-gascity-packages
            home-gascity-gc-home
            home-gascity-cities
            home-gascity-service-type))

;;; Commentary:
;;;
;;; Singleton Gas City supervisor home service.
;;;
;;; Runs 'gc supervisor run' under shepherd.  The supervisor is per-user
;;; (it holds an exclusive flock on <GC_HOME>/supervisor.lock) and reconciles
;;; every city listed in <GC_HOME>/cities.toml — never one shepherd service
;;; per city.
;;;
;;; The service splits its work across two shepherd services so that
;;; 'herd status' stays responsive even on a clean first run:
;;;
;;;   * gascity-init (one-shot): mkdir GC_HOME, 'gc init <path>' iff
;;;     city.toml is missing, 'git clone <url> <path>' iff the rig
;;;     path does not yet exist (gascity has no --git-url flag),
;;;     'gc rig add <path>', and finally write GC_HOME/cities.toml
;;;     listing every declared city.
;;;
;;;   * gascity-supervisor (long-running, requires gascity-init):
;;;     forks 'gc supervisor run'.  The supervisor reads cities.toml
;;;     on startup and reconciles each entry.
;;;
;;; cities.toml is written directly rather than through 'gc register'
;;; because gc register blocks for minutes per city waiting for the
;;; city to become "ready" (which fails when dolt/beads isn't
;;; configured) — see the gc-init-alone-does-not-register memory.
;;;
;;; 'gc supervisor run' itself does NOT start dolt.  With the default
;;; provider = "bd" the user must provide a reachable dolt out-of-band
;;; (e.g. via the existing gastown service).
;;;
;;; Code:

(define-record-type* <gascity-rig-configuration>
  gascity-rig-configuration make-gascity-rig-configuration
  gascity-rig-configuration?
  (path    gascity-rig-path
           (description "Filesystem path to the rig checkout — absolute or
relative to the containing city directory."))
  (git-url gascity-rig-git-url
           (default #f)
           (description "Optional git remote.  When set and PATH does not
exist, the service runs 'git clone <git-url> <path>' before 'gc rig add'
(gascity has no --git-url flag).")))

(define-record-type* <gascity-city-configuration>
  gascity-city-configuration make-gascity-city-configuration
  gascity-city-configuration?
  (path gascity-city-path
        (description "Absolute filesystem path to the city directory.
Created and 'gc init'ed if city.toml is missing."))
  (name gascity-city-name
        (default #f)
        (description "Optional explicit name; otherwise gascity derives one."))
  (rigs gascity-city-rigs
        (default '())
        (description "List of <gascity-rig-configuration> records.")))

(define-record-type* <home-gascity-configuration>
  home-gascity-configuration make-home-gascity-configuration
  home-gascity-configuration?
  (packages home-gascity-packages
            (default (list gascity-next dolt beads-next
                           coreutils guix
                           tmux git grep jq procps sed util-linux lsof))
            (description "List of packages to add to the profile."))
  (gc-home  home-gascity-gc-home
            (default ".gc")
            (description "Supervisor runtime directory relative to $HOME."))
  (dolt-user-name home-gascity-dolt-user-name
                  (default "Gas City")
                  (description "Identity written to ~/.dolt/config_global.json so
that dolt — which beads invokes during 'gc init' and city startup — has a
user.name.  Only seeded if the file does not already exist; existing dolt
configs are preserved."))
  (dolt-user-email home-gascity-dolt-user-email
                   (default "gascity@localhost")
                   (description "Companion email for the seeded
~/.dolt/config_global.json; see dolt-user-name."))
  (cities   home-gascity-cities
            (default '())
            (description "List of <gascity-city-configuration> records.")))

(define (city-spec city)
  "Serialize CITY into a plain list usable inside gexps.  When the
configuration omits an explicit name, default to the path's basename
(matching gc register's own default)."
  (let ((path (gascity-city-path city)))
    (list path
          (or (gascity-city-name city) (basename path))
          (map (lambda (rig)
                 (list (gascity-rig-path rig)
                       (or (gascity-rig-git-url rig) "")))
               (gascity-city-rigs city)))))

(define (with-env gc-home body)
  "Wrap BODY in a start thunk that binds the gascity-wide names HOME,
PROFILE, GC-HOME-ABS, ENV, LOG-FILE and SOCK-FILE.  Both gascity
shepherd services share the same shell-style environment, so the start
thunks differ only in what BODY does once the names are in scope."
  #~(lambda _
      (let* ((home (string-append user-homedir))
             (profile (string-append home "/.guix-home/profile"))
             (gc-home-abs (string-append home "/" #$gc-home))
             (xdg-runtime (or (getenv "XDG_RUNTIME_DIR")
                              (string-append "/run/user/"
                                             (number->string (getuid)))))
             (env (cons*
                   (string-append "GC_HOME=" gc-home-abs)
                   (string-append "PATH=" profile "/bin:" profile "/sbin")
                   (string-append "XDG_RUNTIME_DIR=" xdg-runtime)
                   (string-append "SSL_CERT_DIR=" profile "/etc/ssl/certs")
                   (string-append "SSL_CERT_FILE=" profile
                                  "/etc/ssl/certs/ca-certificates.crt")
                   (string-append "GIT_SSL_CAINFO=" profile
                                  "/etc/ssl/certs/ca-certificates.crt")
                   (user-environment-variables)))
             (log-file (string-append gc-home-abs "/supervisor.log"))
             (sock-file (string-append gc-home-abs "/supervisor.sock")))
        #$body)))

(define (home-gascity-shepherd-services config)
  "Return two shepherd services that together stand up the Gas City
supervisor: gascity-init (one-shot, lays out cities + rigs on disk and
writes GC_HOME/cities.toml directly) and gascity-supervisor (the
long-running daemon, which reads cities.toml on startup and reconciles
each entry).  Writing cities.toml directly avoids 'gc register', which
otherwise blocks for minutes per city waiting for the city to become
'ready'."
  (let* ((gc-home    (home-gascity-gc-home config))
         (cities     (home-gascity-cities config))
         (city-specs (map city-spec cities))
         (gc-bin     (file-append gascity-next "/bin/gc"))
         (git-bin    (file-append git "/bin/git")))
    (list
     (shepherd-service
      (documentation "Lay out each declared Gas City on disk and \
populate GC_HOME/cities.toml so the supervisor reconciles them on startup.")
      (provision '(gascity-init))
      (one-shot? #t)
      (modules '((shepherd support)
                 (ice-9 textual-ports)
                 (guix build utils)))
      (start
       (with-env gc-home
         #~(begin
             (mkdir-p gc-home-abs)
             (for-each
              (lambda (city-spec)
                (let* ((city-path (car city-spec))
                       (rigs      (caddr city-spec))
                       (city-toml (string-append city-path "/city.toml")))
                  (mkdir-p city-path)
                  (unless (file-exists? city-toml)
                    (waitpid
                     (fork+exec-command
                      (list #$gc-bin "init" city-path)
                      #:directory home
                      #:log-file log-file
                      #:environment-variables env)))
                  (for-each
                   (lambda (rig-spec)
                     (let* ((rig-path (car rig-spec))
                            (git-url  (cadr rig-spec))
                            (abs-rig  (if (string-prefix? "/" rig-path)
                                          rig-path
                                          (string-append city-path "/"
                                                         rig-path))))
                       (when (and (not (file-exists? abs-rig))
                                  (not (string-null? git-url)))
                         (waitpid
                          (fork+exec-command
                           (list #$git-bin "clone" git-url abs-rig)
                           #:directory home
                           #:log-file log-file
                           #:environment-variables env)))
                       (let ((toml-text
                              (if (file-exists? city-toml)
                                  (call-with-input-file city-toml
                                    (lambda (p) (get-string-all p)))
                                  "")))
                         (unless (string-contains toml-text abs-rig)
                           (waitpid
                            (fork+exec-command
                             (list #$gc-bin "rig" "add" abs-rig)
                             #:directory city-path
                             #:log-file log-file
                             #:environment-variables env))))))
                   rigs)))
              '#$city-specs)
             ;; Write GC_HOME/cities.toml directly: 'gc register' would
             ;; otherwise block for minutes per city waiting for it to
             ;; become "ready" (which fails when dolt/beads isn't
             ;; configured).  The supervisor reads cities.toml on
             ;; startup and reconciles each entry.
             (call-with-output-file (string-append gc-home-abs "/cities.toml")
               (lambda (port)
                 (for-each
                  (lambda (city-spec)
                    (let ((path (car city-spec))
                          (name (cadr city-spec)))
                      (display "[[cities]]\n" port)
                      (display (string-append "  path = \"" path "\"\n") port)
                      (unless (string-null? name)
                        (display (string-append "  name = \"" name "\"\n")
                                 port))
                      (display "\n" port)))
                  '#$city-specs)))
             #t))))
     (shepherd-service
      (documentation "Run 'gc supervisor run' as the singleton Gas City supervisor.")
      (provision '(gascity-supervisor))
      (requirement '(gascity-init))
      (modules '((shepherd support)
                 (guix build utils)))
      (respawn? #t)
      (respawn-limit #~(cons 3 30))
      (start
       (with-env gc-home
         #~(begin
             (mkdir-p gc-home-abs)
             (fork+exec-command
              (list #$gc-bin "supervisor" "run")
              #:directory home
              #:log-file log-file
              #:environment-variables env))))
      (stop #~(make-kill-destructor))))))

(define (home-gascity-environment-variables config)
  "Export GC_HOME=$HOME/<gc-home> into the user's shell so interactive
'gc' invocations talk to the same supervisor that shepherd starts.
Without this, 'gc supervisor status' would default to ~/.gc and report
'Supervisor is not running' even though shepherd's gascity-supervisor
is healthy at the configured gc-home."
  `(("GC_HOME" . ,(string-append "$HOME/" (home-gascity-gc-home config)))))

(define (home-gascity-activation config)
  "Seed ~/.dolt/config_global.json with a user identity so dolt — which
beads invokes during 'gc init' and city startup — does not refuse to run
with 'dolt identity not configured and git user.name not available'.
Skipped when either ~/.dolt/config_global.json or ~/.gitconfig already
exists: dolt falls back to the git config when its own is missing, so
home-git-service-type satisfies the requirement on its own."
  (let ((user-name  (home-gascity-dolt-user-name config))
        (user-email (home-gascity-dolt-user-email config)))
    (with-imported-modules '((guix build utils))
      #~(begin
          (use-modules (guix build utils))
          (let* ((home (getenv "HOME"))
                 (dolt-dir (string-append home "/.dolt"))
                 (global-json (string-append dolt-dir "/config_global.json"))
                 (gitconfig (string-append home "/.gitconfig")))
            (unless (or (file-exists? global-json)
                        (file-exists? gitconfig))
              (mkdir-p dolt-dir)
              (call-with-output-file global-json
                (lambda (out)
                  (display
                   (string-append
                    "{\"user.name\":\"" #$user-name "\","
                    "\"user.email\":\"" #$user-email "\"}\n")
                   out)))))))))

(define home-gascity-service-type
  (service-type
   (name 'home-gascity)
   (extensions
    (list (service-extension home-profile-service-type
                             home-gascity-packages)
          (service-extension home-shepherd-service-type
                             home-gascity-shepherd-services)
          (service-extension home-environment-variables-service-type
                             home-gascity-environment-variables)
          (service-extension home-activation-service-type
                             home-gascity-activation)))
   (default-value (home-gascity-configuration))
   (description
    "Singleton Gas City supervisor home service.  Runs 'gc supervisor run'
as a shepherd-managed user service that reconciles every city declared via
<gascity-city-configuration> records.  Activation seeds
~/.dolt/config_global.json so dolt — invoked by beads during city startup —
has a user identity.  The supervisor itself does not start dolt; with the
default beads provider 'bd' the user must provide a reachable dolt
out-of-band.")))
