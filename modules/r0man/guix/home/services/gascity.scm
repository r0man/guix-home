(define-module (r0man guix home services gascity)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lsof)
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
;;; The start thunk performs idempotent setup before exec'ing the supervisor:
;;;
;;;   * mkdir GC_HOME (default ~/.gc).
;;;   * For each declared city: 'gc init <path>' iff city.toml is missing.
;;;   * For each declared rig: 'git clone <url> <path>' iff the path does
;;;     not yet exist and git-url is set (gascity has no --git-url flag),
;;;     then 'gc rig add <path>' (run from the city dir; gc resolves the
;;;     containing city from cwd) iff the rig isn't already referenced in
;;;     city.toml.
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
                           tmux git jq procps util-linux lsof))
            (description "List of packages to add to the profile."))
  (gc-home  home-gascity-gc-home
            (default ".gc")
            (description "Supervisor runtime directory relative to $HOME."))
  (cities   home-gascity-cities
            (default '())
            (description "List of <gascity-city-configuration> records.")))

(define (city-spec city)
  "Serialize CITY into a plain list usable inside gexps."
  (list (gascity-city-path city)
        (or (gascity-city-name city) "")
        (map (lambda (rig)
               (list (gascity-rig-path rig)
                     (or (gascity-rig-git-url rig) "")))
             (gascity-city-rigs city))))

(define (home-gascity-shepherd-services config)
  "Return the singleton supervisor shepherd service for CONFIG."
  (let* ((gc-home    (home-gascity-gc-home config))
         (cities     (home-gascity-cities config))
         (city-specs (map city-spec cities))
         (gc-bin     (file-append gascity-next "/bin/gc"))
         (git-bin    (file-append git "/bin/git")))
    (list
     (shepherd-service
      (documentation "Run 'gc supervisor run' as the singleton Gas City supervisor.")
      (provision '(gascity-supervisor))
      (modules '((shepherd support)
                  (ice-9 textual-ports)
                  (guix build utils)))
      (respawn? #t)
      (respawn-limit #~(cons 3 30))
      (start
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
                  (log-file (string-append gc-home-abs "/supervisor.log")))
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
             (fork+exec-command
              (list #$gc-bin "supervisor" "run")
              #:directory home
              #:log-file log-file
              #:environment-variables env))))
      (stop #~(make-kill-destructor))))))

(define home-gascity-service-type
  (service-type
   (name 'home-gascity)
   (extensions
    (list (service-extension home-profile-service-type
                             home-gascity-packages)
          (service-extension home-shepherd-service-type
                             home-gascity-shepherd-services)))
   (default-value (home-gascity-configuration))
   (description
    "Singleton Gas City supervisor home service.  Runs 'gc supervisor run'
as a shepherd-managed user service that reconciles every city declared via
<gascity-city-configuration> records.  The supervisor itself does not start
dolt; with the default beads provider 'bd' the user must provide a reachable
dolt out-of-band.")))
