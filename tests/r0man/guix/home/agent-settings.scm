;;; Tests for the managed coding agent settings (Claude Code, pi).
;;;
;;; The first group checks the managed JSON files and the service
;;; wiring without touching the store.  The second group builds the
;;; activation produced by `json-settings-activation', runs it with
;;; HOME pointing at a scratch directory and checks the merge.  It
;;; needs the Guix daemon and is skipped when none is reachable.

(define-module (test-r0man-home-agent-settings)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module ((guix build utils) #:select (delete-file-recursively))
  #:use-module (guix derivations)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix store)
  #:use-module (ice-9 textual-ports)
  #:use-module (json)
  #:use-module (r0man guix home services agent-tools)
  #:use-module (r0man guix home services claude-code)
  #:use-module (r0man guix home services pi)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64))

(test-begin "r0man-home-agent-settings")

;;; Managed files and service wiring

(define (read-json file)
  (call-with-input-file file json->scm))

(define (managed-json file)
  "Read the managed settings FILE below r0man/guix/home/files."
  (read-json (search-path %load-path
                          (string-append "r0man/guix/home/files/" file))))

(define (activation-extension? type)
  (any (lambda (extension)
         (eq? (service-extension-target extension)
              home-activation-service-type))
       (service-type-extensions type)))

(define claude-settings
  (managed-json "claude-code/settings.json"))

(define pi-settings
  (managed-json "pi/settings.json"))

(test-assert "claude settings are a JSON object"
  (pair? claude-settings))

(test-assert "pi settings are a JSON object"
  (pair? pi-settings))

(test-assert "claude settings do not manage hooks"
  ;; Other tools (e.g. herdr) install hooks into the live file; a
  ;; managed "hooks" key would clobber their arrays on every reconfigure.
  (not (assoc "hooks" claude-settings)))

(test-assert "pi settings do not manage state written by pi"
  (not (assoc "lastChangelogVersion" pi-settings)))

(test-assert "claude-code service merges settings on activation"
  (activation-extension? home-claude-code-service-type))

(test-assert "pi service merges settings on activation"
  (activation-extension? home-pi-service-type))

;;; Merging (needs the daemon)

(define %store
  (false-if-exception (open-connection)))

(define (build-activation target managed)
  "Build a program running the activation that merges MANAGED, a JSON
string, into TARGET below $HOME.  Return its file name."
  (let ((program (program-file
                  "test-agent-settings-activation"
                  (json-settings-activation
                   target (plain-file "managed.json" managed)))))
    (run-with-store %store
      (mlet %store-monad ((drv (lower-object program)))
        (mbegin %store-monad
          (built-derivations (list drv))
          (return (derivation->output-path drv)))))))

(define (call-with-settings-file proc)
  "Call PROC with a scratch home directory and the settings file below
it, then delete the directory."
  (let ((home (mkdtemp (string-append (or (getenv "TMPDIR") "/tmp")
                                      "/agent-settings-XXXXXX"))))
    (dynamic-wind
      (const #t)
      (lambda ()
        (proc home (string-append home "/.agent/settings.json")))
      (lambda ()
        (delete-file-recursively home)))))

(define (run-activation home program)
  "Run PROGRAM with HOME set to HOME and return its exit status."
  (let ((old-home (getenv "HOME")))
    (dynamic-wind
      (lambda () (setenv "HOME" home))
      (lambda () (status:exit-val (system* program)))
      (lambda () (setenv "HOME" old-home)))))

(define (write-file file content)
  (call-with-output-file file
    (lambda (port) (put-string port content))))

(define (merge existing managed)
  "Merge the JSON string MANAGED into a settings file containing the
JSON string EXISTING (or no file when EXISTING is #f).  Return the
merged settings as a Scheme value."
  (call-with-settings-file
   (lambda (home file)
     (when existing
       (mkdir (dirname file))
       (write-file file existing))
     (run-activation home (build-activation ".agent/settings.json" managed))
     (read-json file))))

(define (json-path value . keys)
  (fold (lambda (key value) (assoc-ref value key)) value keys))

(unless %store
  (test-skip 7))

(test-equal "creates missing file and directory"
  "opus"
  (json-path (merge #f "{\"model\": \"opus\"}") "model"))

(test-equal "managed keys win"
  "opus"
  (json-path (merge "{\"model\": \"haiku\"}" "{\"model\": \"opus\"}")
             "model"))

(test-equal "unmanaged keys are kept"
  #t
  (json-path (merge "{\"userAdded\": true}" "{\"model\": \"opus\"}")
             "userAdded"))

(test-equal "nested objects are merged"
  '("bash hook.sh" "1")
  (let ((merged (merge "{\"hooks\": {\"SessionStart\": \"bash hook.sh\"},
                         \"env\": {\"A\": \"0\"}}"
                       "{\"env\": {\"A\": \"1\"}}")))
    (list (json-path merged "hooks" "SessionStart")
          (json-path merged "env" "A"))))

(test-equal "arrays are replaced, not concatenated"
  #("npm:b")
  (json-path (merge "{\"packages\": [\"npm:a\"]}"
                    "{\"packages\": [\"npm:b\"]}")
             "packages"))

(test-equal "file mode is preserved"
  #o600
  (call-with-settings-file
   (lambda (home file)
     (mkdir (dirname file))
     (write-file file "{}")
     (chmod file #o600)
     (run-activation home (build-activation ".agent/settings.json" "{}"))
     (logand #o777 (stat:perms (stat file))))))

(test-equal "invalid existing JSON is left untouched"
  "{ not json"
  (call-with-settings-file
   (lambda (home file)
     (mkdir (dirname file))
     (write-file file "{ not json")
     (run-activation home (build-activation ".agent/settings.json"
                                            "{\"model\": \"opus\"}"))
     (call-with-input-file file get-string-all))))

(test-end "r0man-home-agent-settings")
