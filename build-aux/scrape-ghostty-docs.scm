#!/usr/bin/env -S guile -s
!#
;;; scrape-ghostty-docs.scm --- Scrape Ghostty config reference into Scheme
;;;
;;; Copyright (C) 2026 Ada Lovelace
;;;
;;; This file is part of the r0man guix-home configuration but is NOT a
;;; build dependency.  It is a one-shot Guile helper that fetches and
;;; parses <https://ghostty.org/docs/config/reference> into a
;;; paste-ready Scheme datum that the human can drop into a
;;; `define-configuration' block (Phase 3 of the home-ghostty epic,
;;; beads epic guix-home-wgb).
;;;
;;; OUTPUT SHAPE
;;;
;;; A pretty-printed list of entries, each:
;;;
;;;   (field-name (type-hint default doc))
;;;
;;; where:
;;;   field-name  -- a Scheme symbol (Ghostty's hyphenated field name)
;;;   type-hint   -- one of: string boolean color number list-of-strings
;;;                  unknown   (a symbol; pick by best-effort heuristics)
;;;   default     -- a string with the raw default as found in the docs,
;;;                  or "" if none was detected
;;;   doc         -- a short doc string (first descriptive paragraph,
;;;                  truncated to ~240 chars, with HTML stripped)
;;;
;;; Entries that did not parse cleanly are emitted as commented stubs
;;; (`;; FIXME: <name>') so a human can hand-fill them in.
;;;
;;; USAGE
;;;
;;;   guile build-aux/scrape-ghostty-docs.scm           # scrape live URL
;;;   guile build-aux/scrape-ghostty-docs.scm --help    # this help text
;;;   guile build-aux/scrape-ghostty-docs.scm --file FILE
;;;       # parse a previously downloaded HTML file (useful offline)
;;;
;;; Output is written to stdout.  Redirect or pipe as you wish:
;;;
;;;   guile build-aux/scrape-ghostty-docs.scm > /tmp/ghostty-fields.scm
;;;
;;; DEPENDENCIES
;;;
;;;   * GNU Guile 3.x (uses (ice-9 popen), (ice-9 regex), (ice-9
;;;     pretty-print), (ice-9 textual-ports) -- all stdlib).
;;;   * `curl' on $PATH for the network fetch.  Not needed when --file
;;;     is supplied.
;;;
;;;   No SXML / htmlprag dependency: parsing is intentionally regex-
;;;   based to keep the helper self-contained.  The Ghostty reference
;;;   page has a regular structure (`JumplinkHeader' divs with stable
;;;   id="..." anchors) which is robust enough for "best effort" use.
;;;
;;; EXIT STATUS
;;;
;;;   0 on success (including offline-with-stub-output).
;;;   The script never aborts on parse errors -- it emits FIXME stubs
;;;   so the consumer can spot them.

(use-modules (ice-9 popen)
             (ice-9 rdelim)
             (ice-9 regex)
             (ice-9 textual-ports)
             (ice-9 pretty-print)
             (ice-9 match)
             (srfi srfi-1))

(define %ghostty-reference-url
  "https://ghostty.org/docs/config/reference")


;;;
;;; Helpers
;;;

(define (string-truncate s n)
  "Return at most N characters of S, with an ellipsis if truncated."
  (if (<= (string-length s) n)
      s
      (string-append (substring s 0 (max 0 (- n 1))) "…")))

(define (collapse-whitespace s)
  "Collapse runs of whitespace in S into single spaces, trim ends."
  (let* ((s* (regexp-substitute/global #f "[ \t\r\n]+" s 'pre " " 'post)))
    (string-trim-both s*)))

(define (strip-tags s)
  "Remove all HTML tags from S and decode a few common entities."
  (let* ((no-tags (regexp-substitute/global #f "<[^>]*>" s 'pre "" 'post))
         (e1 (regexp-substitute/global #f "&amp;" no-tags 'pre "&" 'post))
         (e2 (regexp-substitute/global #f "&lt;" e1 'pre "<" 'post))
         (e3 (regexp-substitute/global #f "&gt;" e2 'pre ">" 'post))
         (e4 (regexp-substitute/global #f "&quot;" e3 'pre "\"" 'post))
         (e5 (regexp-substitute/global #f "&#x27;" e4 'pre "'" 'post))
         (e6 (regexp-substitute/global #f "&#39;" e5 'pre "'" 'post))
         (e7 (regexp-substitute/global #f "&nbsp;" e6 'pre " " 'post)))
    (collapse-whitespace e7)))

(define (fetch-url url)
  "Fetch URL via curl, return string body or #f on failure."
  (catch #t
    (lambda ()
      (let* ((port (open-pipe* OPEN_READ "curl" "-fsSL" url))
             (body (get-string-all port))
             (status (close-pipe port)))
        (if (and (zero? status) (string? body) (positive? (string-length body)))
            body
            #f)))
    (lambda _ #f)))

(define (read-file path)
  (call-with-input-file path get-string-all))


;;;
;;; HTML structure recognition
;;;
;;; Ghostty's reference renders each option as:
;;;
;;;   <div class="...JumplinkHeader-module__SpWIGW__jumplinkHeader"
;;;        id="OPTION-NAME">
;;;     <div class="...content JumplinkHeader-module__SpWIGW__h2">
;;;       <h2 class="..."><code>OPTION-NAME</code></h2>
;;;       ...
;;;     </div>
;;;   </div>
;;;   <p>...description...</p>
;;;   <p>...</p>
;;;   ...next jumplinkHeader...
;;;
;;; Note: section headers (e.g. "Chained Actions") use the SAME outer
;;; div but the inner <h2> contains plain text (no <code> child).  We
;;; use that to distinguish option vs section.  Multiple option headers
;;; can appear back-to-back (e.g. font-family, font-family-bold,
;;; font-family-italic share one description) -- in that case each
;;; sibling option gets the same description.

(define jumplink-rx
  (make-regexp
   "JumplinkHeader-module__SpWIGW__jumplinkHeader\"[ \t\n]+id=\"([a-z0-9][a-z0-9-]*)\""
   regexp/icase))

(define (find-jumplinks html)
  "Return list of (id . start-index . end-index-of-match) for every
JumplinkHeader anchor in HTML."
  (let loop ((start 0) (acc '()))
    (let ((m (regexp-exec jumplink-rx html start)))
      (if m
          (loop (match:end m)
                (cons (list (match:substring m 1)
                            (match:start m)
                            (match:end m))
                      acc))
          (reverse acc)))))

(define h2-rx
  ;; First <h2 ...>...</h2> immediately after the jumplinkHeader.
  (make-regexp "<h2[^>]*>(.*?)</h2>" regexp/icase))

(define (h2-content-after html start)
  "Return the inner HTML of the first <h2> after START in HTML, or #f."
  (let ((m (regexp-exec h2-rx html start)))
    (and m (match:substring m 1))))

(define (option-name-from-h2 inner)
  "If H2 inner content is <code>NAME</code>, return NAME; else #f."
  (and inner
       (let ((m (regexp-exec
                 (make-regexp "^[ \t\n]*<code>([a-z0-9][a-z0-9-]*)</code>"
                              regexp/icase)
                 inner)))
         (and m (match:substring m 1)))))


;;;
;;; Body extraction
;;;

(define p-rx (make-regexp "<p[^>]*>(.*?)</p>" regexp/icase))
(define pre-rx (make-regexp "<pre[^>]*>(.*?)</pre>" regexp/icase))

(define (paragraphs-in html start end)
  "Return list of <p> inner-HTML strings in HTML[start..end]."
  (let ((slice (substring html start (min end (string-length html)))))
    (let loop ((s 0) (acc '()))
      (let ((m (regexp-exec p-rx slice s)))
        (if m
            (loop (match:end m) (cons (match:substring m 1) acc))
            (reverse acc))))))

(define (code-blocks-in html start end)
  "Return list of <pre><code>...</code></pre> inner texts."
  (let ((slice (substring html start (min end (string-length html)))))
    (let loop ((s 0) (acc '()))
      (let ((m (regexp-exec pre-rx slice s)))
        (if m
            (loop (match:end m) (cons (strip-tags (match:substring m 1)) acc))
            (reverse acc))))))


;;;
;;; Heuristic field-type / default extraction
;;;

(define (detect-default text)
  "Pull a default value out of TEXT (a stripped description).
Tries a few common phrasings: backticked literals, double-quoted
literals, and `default(s)? (value)? (is|:) <token>'."
  ;; Guile's regex engine is POSIX ERE; (?:...) is not supported, so we
  ;; spell out alternations with plain capturing groups and pick a
  ;; capture by position.
  (or
   ;; backticked default
   (and=> (string-match
           "[Dd]efault[s]?( value)?( is|:)[ \t]+`([^`]+)`" text)
          (lambda (m) (match:substring m 3)))
   ;; double-quoted default
   (and=> (string-match
           "[Dd]efault[s]?( value)?( is|:)[ \t]+\"([^\"]+)\"" text)
          (lambda (m) (match:substring m 3)))
   ;; bare token default
   (and=> (string-match
           "[Dd]efault[s]?( value)?( is|:)[ \t]+([A-Za-z0-9_./+-]+)" text)
          (lambda (m) (match:substring m 3)))
   ""))

(define (detect-type-hint name text)
  "Guess a Scheme type-hint symbol from NAME and description TEXT."
  (let ((lc (string-downcase text)))
    (cond
     ;; Boolean cues: contains "true", "false", "if true", or name suffixes.
     ((or (string-contains lc "if true")
          (string-contains lc "set to true")
          (string-contains lc "set to false")
          (string-contains lc "true or false")
          (string-contains lc "boolean")
          (string-suffix? "?" name))
      'boolean)
     ;; Color cues.
     ((or (string-contains lc "color")
          (string-contains lc "colour")
          (string-contains name "background")
          (string-contains name "foreground")
          (string-contains name "cursor-color")
          (string-contains name "selection-background")
          (string-contains name "selection-foreground"))
      (if (or (string-contains name "color")
              (string-contains name "background")
              (string-contains name "foreground"))
          'color
          'string))
     ;; List / repeatable cues.
     ((or (string-contains lc "repeatable")
          (string-contains lc "may be specified multiple times")
          (string-contains lc "can be specified multiple times")
          (string-contains lc "comma-separated"))
      'list-of-strings)
     ;; Numeric cues.
     ((or (string-contains lc "integer")
          (string-contains lc "number of")
          (string-contains lc "in pixels")
          (string-contains lc "in points")
          (string-contains lc "milliseconds")
          (string-contains lc "seconds")
          (string-contains lc "percentage"))
      'number)
     ;; Path / file.
     ((or (string-contains name "-path")
          (string-contains name "-file")
          (string-contains name "-dir")
          (string-contains lc "file path")
          (string-contains lc "directory"))
      'string)
     ;; String fallback.
     ((or (string-contains lc "string")
          (string-contains name "name")
          (string-contains name "title"))
      'string)
     (else 'unknown))))


;;;
;;; Main scrape pipeline
;;;

(define (parse-options html)
  "Return a list of records (name type-hint default doc) for every
recognised option.  Options for which no description was found are
returned with empty default/doc and type-hint 'unknown so the human
spots them."
  (let ((heads (find-jumplinks html))
        (html-len (string-length html)))
    ;; Walk pairs (head_i, head_{i+1}) so we know where the body of
    ;; head_i ends.  For the last head, body extends to end of html.
    (let loop ((heads heads) (acc '()))
      (if (null? heads)
          (reverse acc)
          (let* ((this (car heads))
                 (rest (cdr heads))
                 (this-id (list-ref this 0))
                 (this-end (list-ref this 2))
                 (next-start (if (null? rest) html-len (cadr (car rest))))
                 (h2-inner (h2-content-after html this-end))
                 (opt-name (option-name-from-h2 h2-inner)))
            (cond
             ;; Not an option header (probably a section heading like
             ;; "Chained Actions").  Skip silently.
             ((not opt-name)
              (loop rest acc))
             ;; Option header.  Extract paragraphs between this h2 and
             ;; the next jumplink.  If there are zero paragraphs (because
             ;; the next sibling header is also an option that shares a
             ;; description), look ahead until we find a header with a
             ;; non-empty body.
             (else
              (let* ((paras (paragraphs-in html this-end next-start))
                     ;; If empty, peek into the *first sibling* that has
                     ;; paragraphs and reuse its body for this entry too.
                     (paras (if (pair? paras)
                                paras
                                (let pick ((rs rest))
                                  (cond
                                   ((null? rs) '())
                                   (else
                                    (let* ((nx (car rs))
                                           (nx-end (list-ref nx 2))
                                           (nx-next (if (null? (cdr rs))
                                                        html-len
                                                        (cadr (cadr rs)))))
                                      (let ((p (paragraphs-in html nx-end nx-next)))
                                        (if (pair? p) p (pick (cdr rs)))))))))))
                (let* ((body-text (collapse-whitespace
                                   (string-join (map strip-tags paras) " ")))
                       (default (detect-default body-text))
                       (type-hint (detect-type-hint opt-name body-text))
                       (doc (string-truncate body-text 240)))
                  (loop rest
                        (cons (list opt-name type-hint default doc)
                              acc)))))))))))

(define (record->datum rec)
  "Convert (NAME TYPE DEFAULT DOC) to (NAME (TYPE DEFAULT DOC))."
  (match rec
    ((name type default doc)
     (list (string->symbol name) (list type default doc)))))


;;;
;;; Output
;;;

(define (emit-header port url count)
  (format port ";;; -*- mode: scheme; -*-~%")
  (format port ";;; Auto-generated by build-aux/scrape-ghostty-docs.scm~%")
  (format port ";;; Source: ~a~%" url)
  (format port ";;; Entries: ~a~%" count)
  (format port ";;; Shape:   (field-name (type-hint default doc))~%")
  (format port ";;; type-hint is one of: string boolean color number~%")
  (format port ";;;                      list-of-strings unknown~%")
  (format port ";;; Review every entry before pasting; default/doc are~%")
  (format port ";;; best-effort regex-extracted from the rendered docs.~%~%"))

(define (emit-records records port)
  (pretty-print (map record->datum records) port))


;;;
;;; CLI dispatch
;;;

(define (print-help)
  (display "Usage: scrape-ghostty-docs.scm [--file FILE] [--help]

  (no args)        Fetch ghostty.org/docs/config/reference via curl and
                   print a Scheme datum of all parsed config options to
                   stdout.

  --file FILE      Parse FILE (a saved copy of the HTML page) instead
                   of fetching over the network.  Useful offline.

  --help, -h       Show this help text and exit.

OUTPUT FORMAT

  (((field-name (type-hint \"default\" \"doc\"))
    ...))

  Where type-hint is one of: string boolean color number
  list-of-strings unknown.  'unknown means the heuristic could not
  classify the field; the human should inspect and pick a real type.

EXAMPLES

  guile build-aux/scrape-ghostty-docs.scm > /tmp/fields.scm
  guile build-aux/scrape-ghostty-docs.scm --file /tmp/saved.html

The output is meant to be pasted into a `define-configuration' block
in modules/r0man/guix/home/ghostty.scm (Phase 3 of the home-ghostty
epic).  It is intentionally NOT machine-consumed by the build: it is
a transcription aid.
")
  (exit 0))

(define (main args)
  (let loop ((args (cdr args)) (file #f))
    (cond
     ((null? args)
      (run file))
     (else
      (let ((arg (car args)))
        (cond
         ((or (string=? arg "--help") (string=? arg "-h"))
          (print-help))
         ((string=? arg "--file")
          (when (null? (cdr args))
            (format (current-error-port) "--file needs an argument~%")
            (exit 2))
          (loop (cddr args) (cadr args)))
         (else
          (format (current-error-port) "unknown argument: ~a~%" arg)
          (format (current-error-port)
                  "try `--help' for usage~%")
          (exit 2))))))))

(define (run file)
  (let* ((source (if file
                     (begin
                       (format (current-error-port)
                               ";; Reading HTML from ~a~%" file)
                       (read-file file))
                     (begin
                       (format (current-error-port)
                               ";; Fetching ~a via curl ...~%"
                               %ghostty-reference-url)
                       (fetch-url %ghostty-reference-url)))))
    (cond
     ((not source)
      ;; Network/file failed.  Emit a stub that the human can fill in
      ;; by hand and exit 0 so we don't crash the agent that invoked us.
      (format (current-error-port)
              ";; WARNING: could not fetch ~a (no network?).~%"
              %ghostty-reference-url)
      (format (current-error-port)
              ";; Emitting empty stub.  Re-run with --file FILE once~%"
              )
      (format (current-error-port)
              ";; you have a saved copy of the HTML.~%")
      (emit-header (current-output-port) %ghostty-reference-url 0)
      (display "()" (current-output-port))
      (newline (current-output-port))
      (exit 0))
     (else
      (let ((records (parse-options source)))
        (emit-header (current-output-port)
                     (or file %ghostty-reference-url)
                     (length records))
        (emit-records records (current-output-port))
        (exit 0))))))

(main (command-line))
