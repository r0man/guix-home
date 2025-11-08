(define-module (r0man guix home claude-code)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (r0man guix packages node)
  #:export (home-claude-code-configuration
            home-claude-code-service-type))

;;; Commentary:
;;;
;;; Home service for Claude Code AI assistant configuration.
;;; Manages ~/.claude/agents, ~/.claude/commands, ~/.claude/skills,
;;; and ~/.claude/settings.json.
;;;
;;; Code:

(define-record-type* <home-claude-code-configuration>
  home-claude-code-configuration make-home-claude-code-configuration
  home-claude-code-configuration?
  (agents home-claude-code-agents
          (default (local-file "files/claude-code/agents" #:recursive? #t))
          (description "Path to agents directory."))
  (commands home-claude-code-commands
            (default (local-file "files/claude-code/commands" #:recursive? #t))
            (description "Path to commands directory."))
  (skills home-claude-code-skills
          (default (local-file "files/claude-code/skills" #:recursive? #t))
          (description "Path to skills directory."))
  (packages home-claude-code-packages
            (default (list node-anthropic-ai-claude-code))
            (description "List of Claude Code packages to install."))
  (settings home-claude-code-settings
            (default (local-file "files/claude-code/settings.json"))
            (description "Path to settings.json file.")))

(define (home-claude-code-files config)
  "Return alist of Claude Code configuration files to deploy."
  `(("bin/container-claude" ,(local-file "files/bin/container-claude" #:recursive? #t))
    (".claude/agents" ,(home-claude-code-agents config))
    (".claude/commands" ,(home-claude-code-commands config))
    (".claude/skills" ,(home-claude-code-skills config))
    (".claude/settings.json" ,(home-claude-code-settings config))))

(define (home-claude-code-profile-packages config)
  "Return list of Claude Code packages to install."
  (home-claude-code-packages config))

(define home-claude-code-service-type
  (service-type
   (name 'home-claude-code)
   (extensions
    (list (service-extension home-files-service-type
                             home-claude-code-files)
          (service-extension home-profile-service-type
                             home-claude-code-profile-packages)))
   (default-value (home-claude-code-configuration))
   (description
    "Install and configure Claude Code for the user.")))
