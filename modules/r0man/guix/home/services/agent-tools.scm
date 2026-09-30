(define-module (r0man guix home services agent-tools)
  #:use-module (gnu packages web)
  #:use-module (guix gexp)
  #:export (%agent-skills
            json-settings-activation))

;;; Commentary:
;;;
;;; Pieces shared by the coding agent services (Claude Code, pi):
;;;
;;; - %agent-skills: the skills directory installed for every agent.
;;;
;;; - json-settings-activation: merge managed JSON settings into a
;;;   writable file under $HOME.  The agents write to their settings
;;;   files themselves (/config, /model, changelog state), so they
;;;   can't be read-only store symlinks.  Instead the activation deep
;;;   merges the managed file into the existing one: managed keys win,
;;;   keys the agent added are kept.
;;;
;;; Code:

(define %agent-skills
  (local-file "../files/skills" "agent-skills" #:recursive? #t))

(define (json-settings-activation target managed)
  "Return a gexp that deep merges the JSON file MANAGED into TARGET, a
file name relative to $HOME.  Objects are merged recursively, managed
values win and arrays are replaced."
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils)
                     (ice-9 popen)
                     (ice-9 textual-ports))
        (let* ((file (string-append (getenv "HOME") "/" #$target))
               (tmp (string-append file ".guix-tmp")))
          (mkdir-p (dirname file))
          (unless (file-exists? file)
            (call-with-output-file file
              (lambda (port) (display "{}\n" port))))
          (let* ((port (open-pipe* OPEN_READ #$(file-append jq "/bin/jq")
                                   "--slurp" ".[0] * .[1]" file #$managed))
                 (json (get-string-all port)))
            (if (and (zero? (status:exit-val (close-pipe port)))
                     (not (string-null? json)))
                (begin
                  (call-with-output-file tmp
                    (lambda (out) (display json out)))
                  (chmod tmp (logand #o7777 (stat:perms (stat file))))
                  (rename-file tmp file))
                (format (current-error-port)
                        "warning: could not merge ~a into ~a~%"
                        #$managed file)))))))
