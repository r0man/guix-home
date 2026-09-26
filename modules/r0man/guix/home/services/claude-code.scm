(define-module (r0man guix home services claude-code)
  #:use-module (gnu home services)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages web)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (r0man guix home services agent-tools)
  #:use-module (r0man guix packages claude)
  #:use-module (r0man guix packages lisp)
  #:use-module (r0man guix packages node)
  #:export (home-claude-code-configuration
            home-claude-code-service-type))

;;; Commentary:
;;;
;;; Home service for Claude Code AI assistant configuration.
;;; Manages ~/.claude/agents and ~/.claude/skills (shared with other
;;; agents, see agent-tools.scm).
;;;
;;; ~/.claude/settings.json stays a regular file, because Claude Code
;;; writes to it (/config, plugin toggles).  The activation only merges
;;; the managed hooks into it: a PostToolUse hook that checks the
;;; delimiter balance of edited Lisp files.  files/claude-code/settings.json
;;; is a reference copy of the remaining settings and is not deployed.
;;;
;;; Code:

(define-record-type* <home-claude-code-configuration>
  home-claude-code-configuration make-home-claude-code-configuration
  home-claude-code-configuration?
  (agents home-claude-code-agents
          (default (local-file "../files/claude-code/agents" #:recursive? #t))
          (description "Path to agents directory."))
  (skills home-claude-code-skills
          (default %agent-skills)
          (description "Path to skills directory."))
  (packages home-claude-code-packages
            (default (list claude-code
                          node-zed-industries-claude-agent-acp
                          parenmedic))
            (description "List of Claude Code packages to install.")))

(define claude-lisp-paren-hook
  ;; PostToolUse hook: read the hook event from stdin and run
  ;; lisp-paren-check on the edited file.  Exit status 2 feeds stderr
  ;; back to Claude.
  (program-file
   "claude-lisp-paren-hook"
   (with-extensions (list guile-json-4)
     #~(begin
         (use-modules (ice-9 popen)
                      (ice-9 textual-ports)
                      (json))
         (let* ((event (false-if-exception (json->scm (current-input-port))))
                (input (and (pair? event) (assoc-ref event "tool_input")))
                (file (and (pair? input) (assoc-ref input "file_path"))))
           (when (and (string? file) (file-exists? file))
             (let* ((port (open-pipe* OPEN_READ #$lisp-paren-check-program file))
                    (output (get-string-all port))
                    (status (status:exit-val (close-pipe port))))
               (when (eqv? status 2)
                 (display output (current-error-port))
                 (display "\nThe edit left unbalanced delimiters. Fix them now.\n"
                          (current-error-port))
                 (exit 2)))))))))

(define (home-claude-code-files config)
  "Return alist of Claude Code configuration files to deploy."
  `(("bin/container-claude" ,(local-file "../files/bin/container-claude" #:recursive? #t))
    ("bin/lisp-paren-check" ,lisp-paren-check-program)
    (".claude/agents" ,(home-claude-code-agents config))
    (".claude/skills" ,(home-claude-code-skills config))))

(define %claude-hooks-filter
  ;; Drop earlier versions of our hook (the store path changes), then
  ;; append the current one.  Everything else is left alone.
  "
.hooks //= {}
| .hooks.PostToolUse //= []
| .hooks.PostToolUse |= map(.hooks |= map(select((.command // \"\")
                                                 | contains(\"claude-lisp-paren-hook\")
                                                 | not)))
| .hooks.PostToolUse |= map(select((.hooks | length) > 0))
| .hooks.PostToolUse += [{matcher: \"Edit|Write|MultiEdit\",
                          hooks: [{type: \"command\", command: $cmd}]}]")

(define (home-claude-code-activation config)
  "Merge the managed hooks into ~/.claude/settings.json."
  #~(begin
      (use-modules (ice-9 popen)
                   (ice-9 textual-ports))
      (let* ((dir (string-append (getenv "HOME") "/.claude"))
             (settings (string-append dir "/settings.json"))
             (tmp (string-append settings ".guix-tmp")))
        (unless (file-exists? dir)
          (mkdir dir))
        (unless (file-exists? settings)
          (call-with-output-file settings
            (lambda (port) (display "{}\n" port))))
        (let* ((port (open-pipe* OPEN_READ #$(file-append jq "/bin/jq")
                                 "--arg" "cmd" #$claude-lisp-paren-hook
                                 #$%claude-hooks-filter settings))
               (json (get-string-all port)))
          (if (and (zero? (status:exit-val (close-pipe port)))
                   (not (string-null? json)))
              (begin
                (call-with-output-file tmp
                  (lambda (out) (display json out)))
                (rename-file tmp settings))
              (format (current-error-port)
                      "warning: could not merge hooks into ~a~%" settings))))))

(define (home-claude-code-profile-packages config)
  "Return list of Claude Code packages to install."
  (home-claude-code-packages config))

(define home-claude-code-service-type
  (service-type
   (name 'home-claude-code)
   (extensions
    (list (service-extension home-activation-service-type
                             home-claude-code-activation)
          (service-extension home-files-service-type
                             home-claude-code-files)
          (service-extension home-profile-service-type
                             home-claude-code-profile-packages)))
   (default-value (home-claude-code-configuration))
   (description
    "Install and configure Claude Code for the user.")))
