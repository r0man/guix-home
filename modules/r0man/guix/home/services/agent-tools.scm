(define-module (r0man guix home services agent-tools)
  #:use-module (guix gexp)
  #:export (%agent-skills))

;;; Commentary:
;;;
;;; Pieces shared by the coding agent services (Claude Code, pi):
;;;
;;; - %agent-skills: the skills directory installed for every agent.
;;;
;;; Code:

(define %agent-skills
  (local-file "../files/skills" "agent-skills" #:recursive? #t))
