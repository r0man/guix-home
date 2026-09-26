(define-module (r0man guix home services agent-tools)
  #:use-module (guix gexp)
  #:use-module (r0man guix packages lisp)
  #:export (%agent-skills
            lisp-paren-check-program))

;;; Commentary:
;;;
;;; Pieces shared by the coding agent services (Claude Code, pi):
;;;
;;; - %agent-skills: the skills directory installed for every agent.
;;;
;;; - lisp-paren-check-program: a delimiter balance checker for Lisp
;;;   files.  Models (especially smaller open-weight ones) often leave
;;;   parentheses unbalanced; the agents run this after every edit and
;;;   feed failures back to the model.  Balance is decided by a
;;;   tokenizer, not by parenmedic, because parenmedic's indentation
;;;   heuristics flag balanced but oddly indented code.  parenmedic's
;;;   diagnosis and diff are only attached as a repair suggestion.
;;;
;;; Code:

(define %agent-skills
  (local-file "../files/skills" "agent-skills" #:recursive? #t))

(define lisp-paren-check-program
  (program-file
   "lisp-paren-check"
   #~(begin
       (setenv "PARENMEDIC" #$(file-append parenmedic "/bin/parenmedic"))
       (load #$(local-file "../files/agents/lisp-paren-check.scm")))))
