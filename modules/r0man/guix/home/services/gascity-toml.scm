(define-module (r0man guix home services gascity-toml)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (derive-beads-prefix
            render-rigs-section
            render-pack-section
            render-agent-section
            render-providers-section
            render-city-toml
            render-cities-toml
            render-supervisor-toml))

;;; Commentary:
;;;
;;; Pure-Scheme TOML rendering helpers for home-gascity-service-type.
;;; All renderers return strings that the activation gexp writes to
;;; disk via the (write-toml-atomically) pattern in gascity.scm.
;;;
;;; The functions in this module are intentionally pure (no gexps, no
;;; #~ syntax) so they can be unit-tested with plain SRFI-64 against
;;; golden strings without spinning up a marionette VM.
;;;
;;; Code:

(define (camel-case-split word)
  "Split WORD into runs whenever a lowercase rune is followed by an
uppercase rune, mirroring gascity's splitCompoundWord
(internal/config/config.go:758-776).  Empty input returns ('').  No
splits possible returns the original word."
  (cond
   ((string-null? word) (list word))
   (else
    (let* ((chars (string->list word))
           (n     (length chars)))
      (let loop ((i 1) (start 0) (acc '()))
        (cond
         ((>= i n)
          (let ((parts (reverse (cons (substring word start n) acc))))
            (if (<= (length parts) 1) (list word) parts)))
         ((and (char-upper-case? (list-ref chars i))
               (not (char-upper-case? (list-ref chars (- i 1)))))
          (loop (+ i 1) i (cons (substring word start i) acc)))
         (else
          (loop (+ i 1) start acc))))))))

(define (string-strip-suffix s suffix)
  "Remove SUFFIX from end of S if present, mirroring Go's
strings.TrimSuffix."
  (let ((sl (string-length s))
        (xl (string-length suffix)))
    (if (and (>= sl xl)
             (string=? (substring s (- sl xl) sl) suffix))
        (substring s 0 (- sl xl))
        s)))

(define (string-fields-by-sep s)
  "Split S on '-' or '_', dropping empty runs.  Mirrors
strings.FieldsFunc with the predicate (r == '-' || r == '_')."
  (let loop ((chars (string->list s))
             (acc-chars '())
             (parts '()))
    (cond
     ((null? chars)
      (reverse (if (null? acc-chars)
                   parts
                   (cons (list->string (reverse acc-chars)) parts))))
     ((or (eqv? (car chars) #\-) (eqv? (car chars) #\_))
      (if (null? acc-chars)
          (loop (cdr chars) '() parts)
          (loop (cdr chars) '()
                (cons (list->string (reverse acc-chars)) parts))))
     (else
      (loop (cdr chars) (cons (car chars) acc-chars) parts)))))

(define (derive-beads-prefix name)
  "Scheme port of gascity's DeriveBeadsPrefix
(internal/config/config.go:728-754).  KEEP IN SYNC with upstream — if
the upstream algorithm changes, gc rig add's reAddNeedsConfigWrite
fires on every shepherd start.  Algorithm:
  1. Strip -py / -go suffixes.
  2. Split on '-' or '_'.
  3. If single part, attempt camelCase split.
  4. If 2+ parts: lowercased first letter of each part.
  5. Else if NAME ≤3 chars: lowercased NAME.
  6. Else: lowercased first 2 chars of NAME.

Uses (substring s 0 (min 2 (string-length s))) so a 1-byte name
doesn't crash."
  (let* ((stripped (string-strip-suffix
                    (string-strip-suffix name "-py") "-go"))
         (parts (string-fields-by-sep stripped))
         (parts (if (= (length parts) 1)
                    (camel-case-split (car parts))
                    parts)))
    (cond
     ((>= (length parts) 2)
      (string-downcase
       (list->string
        (filter-map (lambda (p)
                      (and (not (string-null? p))
                           (string-ref p 0)))
                    parts))))
     (else
      ;; Mirrors Go's `name = TrimSuffix(name, ...)` mutation: the
      ;; fall-through branch uses the stripped value, not the original
      ;; argument.  Important for inputs like "foo-py" where stripped
      ;; is shorter than the input.
      (let ((sl (string-length stripped)))
        (cond
         ((<= sl 3) (string-downcase stripped))
         (else (string-downcase
                (substring stripped 0 (min 2 sl))))))))))

(define (toml-escape-string s)
  "Escape S for TOML basic-string syntax: backslashes, double-quotes,
and control characters.  TOML basic strings are bracketed by '\"'."
  (string-concatenate
   (map (lambda (c)
          (case c
            ((#\\) "\\\\")
            ((#\") "\\\"")
            ((#\newline) "\\n")
            ((#\tab) "\\t")
            ((#\return) "\\r")
            (else (string c))))
        (string->list s))))

(define (toml-string s)
  "Render S as a quoted TOML basic string."
  (string-append "\"" (toml-escape-string s) "\""))

(define (toml-bool b)
  "Render B as the lowercase TOML booleans 'true' / 'false'."
  (if b "true" "false"))

(define (rig-derived-name rig)
  "Return the rig's canonical name.  RIG is an alist of (path name
git-url branch depth)."
  (let ((explicit (assoc-ref rig 'name))
        (path     (assoc-ref rig 'path)))
    (if (and explicit (string? explicit) (not (string-null? explicit)))
        explicit
        (basename path))))

(define (render-rigs-section rigs)
  "Render the [[rigs]] block list for RIGS.  Each entry is an alist
with at minimum 'path; optional 'name (else basename) and 'prefix
(else derived from name).  branch / depth are clone parameters and do
NOT appear here (gascity's Rig struct has no such fields, see
internal/config/config.go:399-445)."
  (string-concatenate
   (map (lambda (rig)
          (let* ((path   (assoc-ref rig 'path))
                 (name   (rig-derived-name rig))
                 (prefix (or (assoc-ref rig 'prefix)
                             (derive-beads-prefix name))))
            (string-append
             "[[rigs]]\n"
             "  name = "   (toml-string name)   "\n"
             "  path = "   (toml-string path)   "\n"
             "  prefix = " (toml-string prefix) "\n"
             "\n")))
        rigs)))

(define (render-pack-section name pack)
  "Render a single [packs.NAME] table.  PACK is an alist with 'source
required, 'ref / 'path optional."
  (let ((source (assoc-ref pack 'source))
        (ref    (assoc-ref pack 'ref))
        (path   (assoc-ref pack 'path)))
    (string-append
     "[packs." name "]\n"
     "  source = " (toml-string source) "\n"
     (if (and ref (string? ref) (not (string-null? ref)))
         (string-append "  ref = " (toml-string ref) "\n")
         "")
     (if (and path (string? path) (not (string-null? path)))
         (string-append "  path = " (toml-string path) "\n")
         "")
     "\n")))

(define (render-agent-section agent)
  "Render a single [[agent]] table.  AGENT is an alist with 'name and
'provider required, 'command optional."
  (let ((name     (assoc-ref agent 'name))
        (provider (assoc-ref agent 'provider))
        (command  (assoc-ref agent 'command)))
    (string-append
     "[[agent]]\n"
     "  name = " (toml-string name) "\n"
     "  provider = " (toml-string provider) "\n"
     (if (and command (string? command) (not (string-null? command)))
         (string-append "  start_command = " (toml-string command) "\n")
         "")
     "\n")))

(define (render-providers-section providers)
  "Render [providers.NAME] tables for the (city-providers …) field.
PROVIDERS is an alist of (name . alist) pairs where the inner alist
holds 'command and any other arbitrary string keys."
  (string-concatenate
   (map (lambda (entry)
          (let* ((name (car entry))
                 (kvs  (cdr entry)))
            (string-append
             "[providers." name "]\n"
             (string-concatenate
              (map (lambda (kv)
                     (string-append
                      "  " (symbol->string (car kv)) " = "
                      (toml-string (cdr kv)) "\n"))
                   kvs))
             "\n")))
        providers)))

(define* (render-city-toml #:key
                           (workspace-name "")
                           (workspace-suspended? #f)
                           (provider-default "")
                           (beads-provider "")
                           (rigs '())
                           (packs '())
                           (agents '())
                           (providers '())
                           (extra-toml ""))
  "Render a managed city.toml from the supplied fields.  WORKSPACE-NAME
is the [workspace] name = ... entry (omitted when empty since gc init
also omits it — site.toml carries the runtime name).  PROVIDER-DEFAULT
populates [workspace] provider; BEADS-PROVIDER populates [beads]
provider.  EXTRA-TOML is appended verbatim at the end so users can
escape into raw TOML for sections we don't model."
  (string-append
   "[workspace]\n"
   (if (and workspace-name (not (string-null? workspace-name)))
       (string-append "  name = " (toml-string workspace-name) "\n")
       "")
   (if (and provider-default (not (string-null? provider-default)))
       (string-append "  provider = " (toml-string provider-default) "\n")
       "")
   (if workspace-suspended?
       "  suspended = true\n"
       "")
   "\n"
   (if (and beads-provider (not (string-null? beads-provider)))
       (string-append "[beads]\n"
                      "  provider = " (toml-string beads-provider) "\n"
                      "\n")
       "")
   (render-providers-section providers)
   (string-concatenate
    (map (lambda (p) (render-pack-section (assoc-ref p 'name) p))
         packs))
   (string-concatenate (map render-agent-section agents))
   (render-rigs-section rigs)
   (if (and extra-toml (not (string-null? extra-toml)))
       (string-append extra-toml
                      (if (string-suffix? "\n" extra-toml) "" "\n"))
       "")))

(define (render-cities-toml cities)
  "Render the GC_HOME/cities.toml.  CITIES is a list of alists with
'path required and 'name optional."
  (string-concatenate
   (map (lambda (city)
          (let ((path (assoc-ref city 'path))
                (name (assoc-ref city 'name)))
            (string-append
             "[[cities]]\n"
             "  path = " (toml-string path) "\n"
             (if (and name (string? name) (not (string-null? name)))
                 (string-append "  name = " (toml-string name) "\n")
                 "")
             "\n")))
        cities)))

(define* (render-supervisor-toml #:key port bind patrol-interval)
  "Render <gc-home>/supervisor.toml.  We always write [supervisor]
port so the user-declared value pre-empts gascity's
seedIsolatedSupervisorConfig auto-seed
(internal/supervisor/config.go:212-239)."
  (string-append
   "[supervisor]\n"
   (if port
       (string-append "  port = "
                      (cond
                       ((number? port) (number->string port))
                       ((string? port) port)
                       (else (error 'render-supervisor-toml
                                    "port must be number or string" port)))
                      "\n")
       "")
   (if (and bind (string? bind) (not (string-null? bind)))
       (string-append "  bind = " (toml-string bind) "\n")
       "")
   (if (and patrol-interval (string? patrol-interval)
            (not (string-null? patrol-interval)))
       (string-append "  patrol_interval = "
                      (toml-string patrol-interval) "\n")
       "")))

;;; gascity-toml.scm ends here
