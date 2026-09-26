;;; lisp-paren-check --- Check delimiter balance of Lisp source files.
;;;
;;; Usage: lisp-paren-check FILE...
;;;
;;; Reads each FILE with a small dialect-aware tokenizer (strings,
;;; comments, character literals) and checks that all delimiters are
;;; balanced and matched.  Files with an unknown extension are skipped.
;;;
;;; Exit status: 0 if every file is balanced, 2 if any file is not, 1 on
;;; usage or I/O errors.  For unbalanced files the diagnosis of
;;; parenmedic (from the PARENMEDIC environment variable, or "parenmedic"
;;; on PATH) is appended as a heuristic repair suggestion.
;;;
;;; Unlike parenmedic, this checker does not look at indentation, so it
;;; never flags balanced but oddly indented code.

(use-modules (ice-9 match)
             (ice-9 popen)
             (ice-9 rdelim)
             (ice-9 textual-ports)
             (srfi srfi-1)
             (srfi srfi-26))

(define %dialects
  ;; extension -> dialect (parenmedic names)
  '(("scm" . scheme) ("ss" . scheme) ("sld" . scheme) ("sls" . scheme)
    ("rkt" . racket) ("rktl" . racket)
    ("lisp" . cl) ("lsp" . cl) ("cl" . cl) ("asd" . cl)
    ("el" . elisp)
    ("clj" . clojure) ("cljs" . clojure) ("cljc" . clojure)
    ("edn" . clojure) ("bb" . clojure)
    ("janet" . janet)))

(define (file-dialect file)
  (let ((dot (string-rindex file #\.)))
    (and dot (assoc-ref %dialects (string-drop file (+ dot 1))))))

(define (dialect-pairs dialect)
  "Return the alist of open -> close delimiters for DIALECT."
  (case dialect
    ;; In Common Lisp and Emacs Lisp, braces (and brackets in CL) are
    ;; ordinary symbol constituents.
    ((cl) '((#\( . #\))))
    ((elisp scheme) '((#\( . #\)) (#\[ . #\])))
    (else '((#\( . #\)) (#\[ . #\]) (#\{ . #\})))))

(define (delimiter? c)
  (or (char-whitespace? c) (memv c '(#\( #\) #\[ #\] #\{ #\} #\" #\; #\'))))

(define (check-string text dialect)
  "Check TEXT written in DIALECT.  Return #f if balanced, otherwise a
list (LINE COLUMN MESSAGE) describing the first problem."
  (define pairs (dialect-pairs dialect))
  (define closers (map cdr pairs))
  (define len (string-length text))
  (define line-comment (if (eq? dialect 'janet) #\# #\;))
  (define block-comments? (memq dialect '(scheme racket cl)))
  (define hash-char? (memq dialect '(scheme racket cl)))
  (define backslash-char? (memq dialect '(clojure elisp)))
  (define pipe-symbols? (memq dialect '(cl racket)))

  ;; Positions are tracked as (line . column), both 1-based.
  (let loop ((i 0) (line 1) (col 1) (stack '()))
    (define (next k)
      ;; Advance K characters, updating line and column.
      (let adv ((j i) (k k) (line line) (col col))
        (cond ((or (zero? k) (>= j len)) (values j line col))
              ((char=? (string-ref text j) #\newline)
               (adv (+ j 1) (- k 1) (+ line 1) 1))
              (else (adv (+ j 1) (- k 1) line (+ col 1))))))
    (define (skip-until pred)
      ;; Advance until PRED holds at the current index (or EOF).
      (let adv ((j i) (line line) (col col))
        (cond ((>= j len) (values j line col))
              ((pred j) (values j line col))
              ((char=? (string-ref text j) #\newline)
               (adv (+ j 1) (+ line 1) 1))
              (else (adv (+ j 1) line (+ col 1))))))
    (define (at j) (and (< j len) (string-ref text j)))

    (if (>= i len)
        (match stack
          (() #f)
          (((c l . k) . rest)
           (list l k (format #f "unclosed '~a' (~a unclosed in total)"
                             c (length stack)))))
        (let ((c (string-ref text i)))
          (cond
           ;; String literal (Janet backtick strings too).
           ((or (char=? c #\") (and (eq? dialect 'janet) (char=? c #\`)))
            (let str ((j (+ i 1)) (l line) (k (+ col 1)))
              (cond ((>= j len)
                     (list line col "unterminated string starting here"))
                    ((char=? (string-ref text j) #\\)
                     (if (and (< (+ j 1) len)
                              (char=? (string-ref text (+ j 1)) #\newline))
                         (str (+ j 2) (+ l 1) 1)
                         (str (+ j 2) l (+ k 2))))
                    ((char=? (string-ref text j) c)
                     (loop (+ j 1) l (+ k 1) stack))
                    ((char=? (string-ref text j) #\newline)
                     (str (+ j 1) (+ l 1) 1))
                    (else (str (+ j 1) l (+ k 1))))))
           ;; Datum comment #; (Scheme, Racket): the next datum is still
           ;; delimited normally, so only skip the prefix.
           ((and block-comments? (char=? c #\#) (eqv? (at (+ i 1)) #\;))
            (loop (+ i 2) line (+ col 2) stack))
           ;; Guile block comment #! ... !# (e.g. script headers).
           ((and (eq? dialect 'scheme) (char=? c #\#) (eqv? (at (+ i 1)) #\!)
                 (let ((n (at (+ i 2))))
                   (or (not n) (char-whitespace? n) (char=? n #\/))))
            (let hb ((j (+ i 2)) (l line) (k (+ col 2)))
              (cond ((>= j len)
                     (list line col "unterminated #! comment starting here"))
                    ((and (char=? (string-ref text j) #\!) (eqv? (at (+ j 1)) #\#))
                     (loop (+ j 2) l (+ k 2) stack))
                    ((char=? (string-ref text j) #\newline)
                     (hb (+ j 1) (+ l 1) 1))
                    (else (hb (+ j 1) l (+ k 1))))))
           ;; Line comment.
           ((char=? c line-comment)
            (call-with-values
                (lambda () (skip-until (lambda (j) (char=? (string-ref text j) #\newline))))
              (lambda (j l k) (loop j l k stack))))
           ;; Nestable block comment #| ... |#.
           ((and block-comments? (char=? c #\#) (eqv? (at (+ i 1)) #\|))
            (let blk ((j (+ i 2)) (l line) (k (+ col 2)) (depth 1))
              (cond ((>= j len)
                     (list line col "unterminated block comment starting here"))
                    ((and (char=? (string-ref text j) #\|) (eqv? (at (+ j 1)) #\#))
                     (if (= depth 1)
                         (loop (+ j 2) l (+ k 2) stack)
                         (blk (+ j 2) l (+ k 2) (- depth 1))))
                    ((and (char=? (string-ref text j) #\#) (eqv? (at (+ j 1)) #\|))
                     (blk (+ j 2) l (+ k 2) (+ depth 1)))
                    ((char=? (string-ref text j) #\newline)
                     (blk (+ j 1) (+ l 1) 1 depth))
                    (else (blk (+ j 1) l (+ k 1) depth)))))
           ;; Character literal #\x (Scheme, Racket, Common Lisp).
           ((and hash-char? (char=? c #\#) (eqv? (at (+ i 1)) #\\))
            (call-with-values (lambda () (next 3))
              (lambda (j l k) (loop j l k stack))))
           ;; Backslash escape / character literal (Clojure, Emacs Lisp).
           ((and backslash-char? (char=? c #\\))
            (call-with-values (lambda () (next 2))
              (lambda (j l k) (loop j l k stack))))
           ;; Emacs Lisp character literal ?x or ?\x at token start.
           ((and (eq? dialect 'elisp) (char=? c #\?)
                 (or (= i 0) (delimiter? (string-ref text (- i 1)))))
            (call-with-values
                (lambda () (next (if (eqv? (at (+ i 1)) #\\) 3 2)))
              (lambda (j l k) (loop j l k stack))))
           ;; |quoted symbol| (Common Lisp, Racket).
           ((and pipe-symbols? (char=? c #\|))
            (let sym ((j (+ i 1)) (l line) (k (+ col 1)))
              (cond ((>= j len)
                     (list line col "unterminated |symbol| starting here"))
                    ((char=? (string-ref text j) #\\) (sym (+ j 2) l (+ k 2)))
                    ((char=? (string-ref text j) #\|)
                     (loop (+ j 1) l (+ k 1) stack))
                    ((char=? (string-ref text j) #\newline)
                     (sym (+ j 1) (+ l 1) 1))
                    (else (sym (+ j 1) l (+ k 1))))))
           ;; Opening delimiter.
           ((assv c pairs)
            (loop (+ i 1) line (+ col 1) (cons (cons* c line col) stack)))
           ;; Closing delimiter.
           ((memv c closers)
            (match stack
              (()
               (list line col (format #f "unexpected '~a' with no matching opener" c)))
              (((o l . k) . rest)
               (if (char=? (assv-ref pairs o) c)
                   (loop (+ i 1) line (+ col 1) rest)
                   (list line col
                         (format #f "'~a' does not match '~a' opened at ~a:~a"
                                 c o l k))))))
           (else
            (call-with-values (lambda () (next 1))
              (lambda (j l k) (loop j l k stack)))))))))

(define (parenmedic-report file dialect)
  "Return parenmedic's diagnosis and suggested diff for FILE, or #f."
  (define parenmedic (or (getenv "PARENMEDIC") "parenmedic"))
  (define (run . args)
    ;; Return the command's output, or #f if it failed or printed nothing.
    (let ((out (false-if-exception
                (with-error-to-port (%make-void-port "w")
                  (lambda ()
                    (let* ((port (apply open-pipe* OPEN_READ parenmedic args))
                           (out (get-string-all port)))
                      (close-pipe port)
                      out))))))
      (and (string? out) (not (string-null? (string-trim-both out))) out)))
  (let* ((dialect-arg (format #f "--dialect=~a" dialect))
         (diag (run "diagnose" "--format=gcc" dialect-arg file))
         (diff (let ((out (run "fix" "--diff" dialect-arg file)))
                 ;; parenmedic ends every diff with this confusing trailer.
                 (and out
                      (string-join
                       (remove (cut string=? <> "No differences found")
                               (string-split out #\newline))
                       "\n")))))
    (and (or diag diff)
         (string-append
          "\nparenmedic suggestion (heuristic, based on indentation -- "
          "verify before applying):\n"
          (or diag "")
          (if diff (string-append "\n" diff) "")
          (format #f "\nApply with: parenmedic fix -i ~a ~a\n"
                  dialect-arg file)))))

(define (check-file file)
  "Check FILE.  Return #t if it is balanced or not a Lisp file."
  (let ((dialect (file-dialect file)))
    (if (not dialect)
        #t
        (match (check-string (call-with-input-file file get-string-all) dialect)
          (#f #t)
          ((line col message)
           (format #t "~a:~a:~a: error: ~a~%" file line col message)
           (let ((report (parenmedic-report file dialect)))
             (when report (display report)))
           #f)))))

(define (main args)
  (match args
    ((_ files ..1)
     (catch 'system-error
       (lambda ()
         (exit (if (every identity (map check-file files)) 0 2)))
       (lambda (key . rest)
         (format (current-error-port) "lisp-paren-check: ~a~%"
                 (apply format #f (cadr rest) (caddr rest)))
         (exit 1))))
    (_
     (format (current-error-port) "Usage: lisp-paren-check FILE...~%")
     (exit 1))))

(main (command-line))
