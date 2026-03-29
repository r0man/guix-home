;;; GNU Guix Home container service.
;;; Copyright © 2025 Roman Scherer <roman@burningswell.com>
;;;
;;; This module provides a home service type that runs Guix Home
;;; environments in Linux namespace containers.  Each container has its
;;; own nested Shepherd managing home services.

(define-module (r0man guix services home-container)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages base)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system file-systems)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix profiles)
  #:use-module (guix records)
  #:use-module (guix self)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (home-container-configuration
            home-container-configuration?
            home-container-name
            home-container-home
            home-container-network?
            home-container-share
            home-container-expose
            home-container-command
            home-container-auto-start?
            home-container-respawn?
            home-container-respawn-limit
            home-container-log-file
            home-container-environment-variables
            home-container-home-directory
            home-container-user-name
            home-container-user-gecos
            home-container-uid
            home-container-gid
            home-container-system-profile
            home-container-service-type))


;;;
;;; Configuration record.
;;;

(define-record-type* <home-container-configuration>
  home-container-configuration make-home-container-configuration
  home-container-configuration?
  ;; Identity
  (name          home-container-name)                         ;string
  (home          home-container-home)                         ;<home-environment>
  ;; Container options (parity with `guix home container')
  (network?      home-container-network?      (default #t))   ;boolean
  (share         home-container-share         (default '()))  ;list of <file-system-mapping>
  (expose        home-container-expose        (default '()))  ;list of <file-system-mapping>
  (command       home-container-command       (default #f))   ;list of strings or #f
  ;; Service options
  (auto-start?   home-container-auto-start?   (default #t))   ;boolean
  (respawn?      home-container-respawn?      (default #t))   ;boolean
  (respawn-limit home-container-respawn-limit                 ;pair (count . seconds)
                 (default #~(cons 5 60)))
  (log-file      home-container-log-file      (default #f))   ;string or #f
  (environment-variables home-container-environment-variables ;alist
                         (default '()))
  ;; User identity — must be specified explicitly because Guix
  ;; evaluates defaults at build time where (getuid) returns 0.
  (home-directory home-container-home-directory)               ;string (required)
  (user-name     home-container-user-name     (default #f))    ;string or #f (derived from home-directory)
  (user-gecos    home-container-user-gecos    (default ""))    ;string
  (uid           home-container-uid           (default 1000))  ;integer
  (gid           home-container-gid           (default 1000))  ;integer
  (system-profile home-container-system-profile               ;profile or #f
                  (default #f)))


;;;
;;; Helpers.
;;;

;; Select (guix ...) and (gnu ...) modules, except (guix config).
;; Not exported from any public Guix module.
(define not-config?
  (match-lambda
    (('guix 'config) #f)
    (('guix _ ...) #t)
    (('gnu _ ...) #t)
    (_ #f)))

(define %default-system-profile
  ;; Minimal profile providing /bin/sh, env, coreutils — needed by the
  ;; activation script which runs "source setup-environment && env -0"
  ;; via open-input-pipe → popen → /bin/sh.
  (profile
   (name "home-container-system-profile")
   (content (packages->manifest (list bash-minimal coreutils)))))


;;;
;;; Container script generation.
;;;

(define (home-container-script config)
  "Return a program-file that launches a container for CONFIG."
  (match-record config <home-container-configuration>
    (name home network? share expose command
     environment-variables home-directory user-name user-gecos
     uid gid system-profile)

    (let* ((user-name (or user-name (basename home-directory)))
           (profile  (or system-profile %default-system-profile))

           ;; Serialize file-system-mapping records as plain data for gexp
           ;; embedding.  Records are not gexp-serializable.
           (mapping->spec
            (lambda (m)
              (list (file-system-mapping-source m)
                    (file-system-mapping-target m)
                    (file-system-mapping-writable? m))))
           (user-mapping-specs
            (map mapping->spec (append share expose)))
           (network-mapping-specs
            (if network?
                (filter-map (lambda (m)
                              (and (file-exists?
                                    (file-system-mapping-source m))
                                   (mapping->spec m)))
                            %network-file-mappings)
                '())))

      (program-file
       (string-append "run-home-container-" name)
       (with-extensions (list guile-gcrypt)
         (with-imported-modules
             `(((guix config) => ,(make-config.scm))
               ,@(source-module-closure
                  '((gnu build accounts)
                    (gnu build linux-container)
                    (gnu system file-systems)
                    (guix build utils)
                    (guix profiles))
                  #:select? not-config?))
           #~(begin
               (use-modules (gnu build accounts)
                            (gnu build linux-container)
                            (gnu system file-systems)
                            (guix build utils)
                            ((guix profiles) #:select (load-profile))
                            (ice-9 match)
                            (srfi srfi-1))

               (define home-dir #$home-directory)
               (define uid #$uid)
               (define gid #$gid)
               (define xdg-runtime
                 (string-append "/run/user/" (number->string uid)))

               ;; Reconstruct <file-system> records from serialized mapping
               ;; specs.
               (define (mapping-spec->bind-mount spec)
                 (match spec
                   ((source target writable?)
                    (file-system-mapping->bind-mount
                     (file-system-mapping
                      (source source)
                      (target target)
                      (writable? writable?))))))

               ;; Build the complete list of container file systems.
               (define container-file-systems
                 (append
                  ;; /gnu/store — read-only bind mount.
                  (list (file-system
                          (device "/gnu/store")
                          (mount-point "/gnu/store")
                          (type "none")
                          (flags '(bind-mount read-only))
                          (check? #f)))
                  ;; Writable /tmp.
                  (list (file-system
                          (device "none")
                          (mount-point "/tmp")
                          (type "tmpfs")
                          (flags '(no-suid no-dev))
                          (options "mode=755,size=10%")
                          (check? #f)))
                  ;; XDG_RUNTIME_DIR — for Shepherd socket.
                  (list (file-system
                          (device "none")
                          (mount-point xdg-runtime)
                          (type "tmpfs")
                          (options "size=10%,mode=700")
                          (check? #f)))
                  ;; Writable, ephemeral home directory.
                  (list (file-system
                          (device "none")
                          (mount-point home-dir)
                          (type "tmpfs")
                          (options "mode=700")
                          (check? #f)))
                  ;; Network configuration files (when sharing host network).
                  (map mapping-spec->bind-mount '#$network-mapping-specs)
                  ;; User-specified share/expose mappings.
                  (map mapping-spec->bind-mount '#$user-mapping-specs)))

               (call-with-container container-file-systems
                 (lambda ()
                   ;; Load system profile for /bin/sh, env, coreutils.
                   (load-profile #$profile)

                   (setenv "HOME" home-dir)
                   (setenv "XDG_RUNTIME_DIR" xdg-runtime)

                   ;; User-specified environment variables.
                   (for-each (match-lambda
                               ((key . value) (setenv key value)))
                             '#$environment-variables)

                   ;; Activate the home environment.  This starts the
                   ;; nested Shepherd as a daemon (it daemonizes itself).
                   (setenv "GUIX_NEW_HOME" #$home)
                   (primitive-load
                    (string-append #$home "/activate"))
                   (setenv "GUIX_NEW_HOME" #f)
                   (chdir home-dir)

                   #$(if command
                         ;; Mode 2: exec the specified command.  Shepherd
                         ;; is already running in the background.
                         #~(apply execl (car '#$command) '#$command)
                         ;; Mode 1: keep the container alive.  The nested
                         ;; Shepherd runs as a daemon.  Loop on pause until
                         ;; a signal interrupts us (SIGTERM from outer
                         ;; Shepherd).  When we exit, the PID namespace
                         ;; collapses and all container processes are killed.
                         #~(let loop ()
                             (catch 'system-error
                               (lambda () (pause) (loop))
                               (lambda args (loop))))))

                 #:namespaces (if #$network?
                                  (delq 'net %namespaces)
                                  %namespaces)
                 #:child-is-pid1? #f
                 #:guest-uid uid
                 #:guest-gid gid
                 #:populate-file-system
                 (lambda ()
                   ;; Create essential files before root is made read-only.
                   (mkdir-p "/etc")
                   (write-passwd
                    (list (password-entry
                           (name #$user-name)
                           (uid uid)
                           (gid gid)
                           (real-name #$user-gecos)
                           (directory home-dir)
                           (shell "/bin/sh"))))
                   (write-group
                    (list (group-entry
                           (name "users")
                           (gid gid))
                          (group-entry
                           (name "overflow")
                           (gid 65534))))
                   ;; Minimal /etc/hosts for localhost resolution.
                   (unless (file-exists? "/etc/hosts")
                     (call-with-output-file "/etc/hosts"
                       (lambda (port)
                         (format port "127.0.0.1 localhost~%::1 localhost~%"))))
                   ;; Create /bin/sh — required by the activation
                   ;; script's open-input-pipe (popen → /bin/sh).
                   (mkdir-p "/bin")
                   (symlink #$(file-append bash-minimal "/bin/sh")
                            "/bin/sh")
                   (mkdir-p "/tmp"))))))))))


;;;
;;; Shepherd service generation.
;;;

(define (home-container-shepherd-services configs)
  "Return a list of shepherd services for CONFIGS."
  (map (lambda (config)
         (match-record config <home-container-configuration>
           (name auto-start? respawn? respawn-limit log-file)
           (shepherd-service
            (documentation
             (format #f "Home environment container '~a'." name))
            (provision
             (list (symbol-append 'home-container-
                                  (string->symbol name))))
            (modules '((shepherd support)))
            (auto-start? auto-start?)
            (respawn? respawn?)
            (respawn-limit respawn-limit)
            (start #~(make-forkexec-constructor
                      (list #$(home-container-script config))
                      #:log-file
                      #$(or log-file
                            #~(string-append
                               %user-log-dir
                               "/home-container-"
                               #$name ".log"))))
            (stop #~(make-kill-destructor)))))
       configs))


;;;
;;; Activation.
;;;

(define (home-container-activation configs)
  "Return a gexp that creates directories needed by the container services."
  #~(begin #t))


;;;
;;; Service type.
;;;

(define home-container-service-type
  (service-type
   (name 'home-container)
   (extensions
    (list (service-extension home-shepherd-service-type
                             home-container-shepherd-services)
          (service-extension home-profile-service-type
                             (const '()))))
   (compose concatenate)
   (extend append)
   (default-value '())
   (description
    "Run Guix Home environments in Linux namespace containers.
Each container has its own user Shepherd managing home services.")))
