(define-module (r0man guix home services claude-code)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (r0man guix home services agent-tools)
  #:use-module (r0man guix packages claude)
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
;;; writes to it (/config, /model, plugin toggles).  The activation
;;; merges files/claude-code/settings.json into it: managed keys win,
;;; everything else (e.g. hooks installed by other tools) is kept.
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
                          node-zed-industries-claude-agent-acp))
            (description "List of Claude Code packages to install."))
  (settings home-claude-code-settings
            (default (local-file "../files/claude-code/settings.json"
                                 "claude-code-settings.json"))
            (description "Settings merged into ~/.claude/settings.json.")))

(define (home-claude-code-files config)
  "Return alist of Claude Code configuration files to deploy."
  `(("bin/container-claude" ,(local-file "../files/bin/container-claude" #:recursive? #t))
    (".claude/agents" ,(home-claude-code-agents config))
    (".claude/skills" ,(home-claude-code-skills config))))

(define (home-claude-code-activation config)
  "Merge the managed settings into ~/.claude/settings.json."
  (json-settings-activation ".claude/settings.json"
                            (home-claude-code-settings config)))

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
