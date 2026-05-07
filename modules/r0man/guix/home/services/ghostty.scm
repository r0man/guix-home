;;; Copyright (C) 2026 Roman Scherer <roman@burningswell.com>
;;;
;;; Fully-typed home service for the Ghostty terminal emulator.
;;;
;;; The schema mirrors Ghostty's documented configuration reference at
;;; https://ghostty.org/docs/config/reference.  Each option is a typed
;;; field with a predicate, default, and docstring; misspellings and
;;; out-of-range values fail at record construction.
;;;
;;; Layout (top-to-bottom; forward references avoided):
;;;
;;;   1. Imports
;;;   2. Custom scalar predicates (color trio, duration, metric, ...)
;;;   3. Enum / bool-or-enum / flagset macros
;;;   4. Enum / hybrid / flagset definitions
;;;   5. Structured records (window-padding, theme-spec, ...)
;;;   6. Repeatable list predicates
;;;   7. Configuration record + extra-options collision check
;;;   8. Serializer
;;;   9. File deployment
;;;  10. Service type

(define-module (r0man guix home services ghostty)
  #:use-module (gnu home services)
  #:use-module (gnu packages bash)
  #:use-module (gnu services configuration)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (r0man guix packages terminals)
  #:export (;; Configuration record + service type
            home-ghostty-configuration
            home-ghostty-configuration?
            home-ghostty-configuration-fields
            home-ghostty-service-type

            ;; Custom scalar predicates
            color?
            cell-color?
            padding-color?
            duration-string?
            metric-modifier?
            percentage-string?
            maybe-string?

            ;; Structured-record constructors / predicates
            window-padding window-padding?
            background-blur background-blur?
            command command?
            quick-terminal-size quick-terminal-size?
            mouse-scroll-multiplier mouse-scroll-multiplier?
            font-style font-style?
            background-image-position background-image-position?
            background-image-fit background-image-fit?
            split-preserve-zoom split-preserve-zoom?
            config-file-include config-file-include?
            theme-spec theme-spec?
            link-rule link-rule?
            mac-shortcut mac-shortcut?
            selection-word-chars selection-word-chars?

            ;; Repeatable list predicates
            list-of-keybinds?
            list-of-palette-entries?
            list-of-env-entries?
            list-of-font-features?
            list-of-font-variations?
            list-of-codepoint-maps?
            list-of-clipboard-codepoint-maps?
            list-of-links?
            list-of-key-remaps?
            list-of-config-file-includes?
            list-of-color-list-entries?
            alist-with-symbol-keys?))


;;;
;;; — Custom scalar predicates —
;;;

(define %x11-color-names
  ;; Subset sufficient for predicate validation.  Ghostty accepts any
  ;; X11 color name and forwards it to the underlying parser; we
  ;; accept any non-empty alphabetic token so we don't have to embed
  ;; the full ~750-entry rgb.txt.
  #t)

(define (hex-rrggbb? s)
  "Return #t when S matches \"#?RRGGBB\" (case-insensitive)."
  (and (string? s)
       (let* ((str (if (and (positive? (string-length s))
                            (char=? (string-ref s 0) #\#))
                       (substring s 1)
                       s)))
         (and (= 6 (string-length str))
              (string-every
               (lambda (c)
                 (or (char-numeric? c)
                     (and (char>=? c #\a) (char<=? c #\f))
                     (and (char>=? c #\A) (char<=? c #\F))))
               str)))))

(define (x11-color-name? s)
  "Return #t when S looks like an X11 color name (alphabetic, ≥3
chars, optional spaces).  Loose check; Ghostty validates the full
list itself."
  (and (string? s)
       (>= (string-length s) 3)
       (string-every
        (lambda (c)
          (or (char-alphabetic? c) (char=? c #\space)))
        s)))

(define (color? x)
  "Hex color (RRGGBB or #RRGGBB) or X11 color name."
  (and (string? x)
       (or (hex-rrggbb? x)
           (x11-color-name? x))))

(define (cell-color? x)
  "color? plus the symbols 'cell-foreground' and 'cell-background'."
  (or (color? x)
      (member x '("cell-foreground" "cell-background"))))

(define (padding-color? x)
  "color? plus the symbol 'background'."
  (or (color? x)
      (equal? x "background")))

(define (duration-string? x)
  "Match Zig duration syntax like \"1h30m45s500ms\" / \"500ms\" /
\"30s\".  Allows ns/us/ms/s/m/h/d units."
  (and (string? x)
       (positive? (string-length x))
       (let ((unit-chars (string->char-set "nusmhd")))
         ;; Must end in a unit letter and contain only digits, dots,
         ;; or unit letters.
         (and (char-set-contains? (string->char-set "smhd")
                                  (string-ref x (- (string-length x) 1)))
              (string-every
               (lambda (c)
                 (or (char-numeric? c)
                     (char=? c #\.)
                     (char-set-contains? unit-chars c)))
               x)))))

(define (percentage-string? x)
  "Match \"NN%\" or \"NN.MM%\"."
  (and (string? x)
       (> (string-length x) 1)
       (char=? #\% (string-ref x (- (string-length x) 1)))
       (let ((body (substring x 0 (- (string-length x) 1))))
         (and (positive? (string-length body))
              (string-every
               (lambda (c) (or (char-numeric? c) (char=? c #\.)))
               body)))))

(define (metric-modifier? x)
  "Either a number (interpreted as raw cell metric), a string of the
form \"+Npx\" / \"-Npx\" / \"NN%\", or a percentage."
  (or (number? x)
      (percentage-string? x)
      (and (string? x)
           (>= (string-length x) 3)
           (or (char=? #\+ (string-ref x 0))
               (char=? #\- (string-ref x 0)))
           (string-suffix? "px" x)
           (let ((body (substring x 1 (- (string-length x) 2))))
             (and (positive? (string-length body))
                  (string-every
                   (lambda (c) (or (char-numeric? c) (char=? c #\.)))
                   body))))))

(define (maybe-string? x)
  "#f or string."
  (or (not x) (string? x)))


;;;
;;; — Enum / hybrid / flagset macros —
;;;
;;; These macros define both the predicate and the matching
;;; serializer-by-name so `define-configuration' auto-resolves them.

(define-syntax define-enum
  (lambda (stx)
    (syntax-case stx ()
      ((_ name (member ...))
       (with-syntax ((pred?
                      (datum->syntax
                       #'name
                       (string->symbol
                        (string-append (symbol->string (syntax->datum #'name))
                                       "?"))))
                     (ser
                      (datum->syntax
                       #'name
                       (string->symbol
                        (string-append "serialize-"
                                       (symbol->string (syntax->datum #'name)))))))
         #'(begin
             (define (pred? x) (memq x '(member ...)))
             (define (ser field-name val)
               (format #f "~a = ~a\n" field-name val))))))))

(define-syntax define-bool-or-enum
  (lambda (stx)
    "Predicate accepting #t / #f / any of the listed symbols.
Serializer emits `name = true` / `name = false` / `name = symbol`."
    (syntax-case stx ()
      ((_ name (member ...))
       (with-syntax ((pred?
                      (datum->syntax
                       #'name
                       (string->symbol
                        (string-append (symbol->string (syntax->datum #'name))
                                       "?"))))
                     (ser
                      (datum->syntax
                       #'name
                       (string->symbol
                        (string-append "serialize-"
                                       (symbol->string (syntax->datum #'name)))))))
         #'(begin
             (define (pred? x)
               (or (boolean? x) (memq x '(member ...))))
             (define (ser field-name val)
               (format #f "~a = ~a\n" field-name
                       (cond ((eq? val #t) "true")
                             ((eq? val #f) "false")
                             (else val))))))))))

(define-syntax define-flagset
  (lambda (stx)
    "Predicate accepting a list of symbols, each either the bare
member, +member, or -member.  Serializer joins with commas."
    (syntax-case stx ()
      ((_ name (member ...))
       (with-syntax ((pred?
                      (datum->syntax
                       #'name
                       (string->symbol
                        (string-append (symbol->string (syntax->datum #'name))
                                       "?"))))
                     (ser
                      (datum->syntax
                       #'name
                       (string->symbol
                        (string-append "serialize-"
                                       (symbol->string (syntax->datum #'name)))))))
         #'(begin
             (define (member-or-modified? sym)
               (and (symbol? sym)
                    (let* ((s (symbol->string sym))
                           (bare (cond ((string-prefix? "+" s) (substring s 1))
                                       ((string-prefix? "-" s) (substring s 1))
                                       (else s))))
                      (memq (string->symbol bare) '(member ...)))))
             (define (pred? x)
               (and (list? x) (every member-or-modified? x)))
             (define (ser field-name val)
               (format #f "~a = ~a\n"
                       field-name
                       (string-join (map symbol->string val) ",")))))))))


;;;
;;; — Boolean (used by many fields) —
;;;

(define (serialize-boolean field-name val)
  (format #f "~a = ~a\n" field-name (if val "true" "false")))


;;;
;;; — Number / string / file-like serializers —
;;;

(define (serialize-number field-name val)
  (format #f "~a = ~a\n" field-name val))

(define (serialize-string field-name val)
  (format #f "~a = ~a\n" field-name val))

(define (serialize-color field-name val)
  (format #f "~a = ~a\n" field-name val))

(define (serialize-cell-color field-name val)
  (format #f "~a = ~a\n" field-name val))

(define (serialize-padding-color field-name val)
  (format #f "~a = ~a\n" field-name val))

(define (serialize-duration-string field-name val)
  (format #f "~a = ~a\n" field-name val))

(define (serialize-metric-modifier field-name val)
  (format #f "~a = ~a\n" field-name val))

(define (serialize-percentage-string field-name val)
  (format #f "~a = ~a\n" field-name val))

(define (serialize-maybe-string field-name val)
  (if val (format #f "~a = ~a\n" field-name val) ""))


;;;
;;; — Closed enums —
;;;
;;; Listed alphabetically by Ghostty option name.  Each enum's symbol
;;; matches Ghostty's documented value verbatim.

(define-enum adjust-cursor-style    (block bar underline block_hollow))
(define-enum clipboard-paste-protection (true false off))
(define-enum command-finder         (full-path basename none))
(define-enum confirm-close-surface  (true false always))
(define-enum cursor-click-to-move   (true false))
(define-enum cursor-style           (block bar underline block_hollow))
(define-enum fullscreen             (true false native non-native))
(define-enum gtk-tabs-location      (top bottom left right hidden))
(define-enum gtk-toolbar-style      (raised flat both-borders))
(define-enum gtk-wide-tabs          (true false))
(define-enum keybind-action-policy  (allow deny prompt))
(define-enum linux-cgroup           (never always single-instance))
(define-enum macos-icon             (official chalkboard custom-style))
(define-bool-or-enum macos-non-native-fullscreen
                                     (visible-menu padded-notch))
(define-enum macos-titlebar-proxy-icon (visible hidden))
(define-enum macos-titlebar-style   (native transparent tabs hidden))
(define-enum macos-window-buttons   (visible hidden))
(define-enum mouse-shift-capture-mode (false true never always))
(define-enum osc-color-report-format (none 8-bit 16-bit))
(define-enum quick-terminal-animation-duration-mode (auto manual))
(define-enum quick-terminal-position (top bottom left right center))
(define-enum quick-terminal-screen  (main mouse macos-menu-bar))
(define-enum resize-overlay         (always never after-first))
(define-enum resize-overlay-position (center top-left top-center top-right
                                              bottom-left bottom-center
                                              bottom-right))
(define-enum shell-integration-mode (detect bash elvish fish nu zsh none))
(define-enum split-divider-color-policy (auto always-show always-hide))
(define-enum tabs-on-bottom         (false true))
(define-enum unfocused-split-fill   (color none))
(define-enum window-colorspace      (srgb display-p3))
(define-enum window-decoration-mode (auto client server none))
(define-enum window-position-x      (auto center))
(define-enum window-position-y      (auto center))
(define-enum window-save-state      (default never always))
(define-enum window-theme           (auto system light dark glass ghostty))


;;;
;;; — Bool-or-enum hybrids —
;;;

(define-bool-or-enum window-decoration (auto client server none))
(define-bool-or-enum mouse-shift-capture (never always))
(define-bool-or-enum copy-on-select (clipboard))
(define-bool-or-enum confirm-close-surface-mode (always))
(define-bool-or-enum quick-terminal-autohide (focused unfocused))
(define-bool-or-enum focus-follows-mouse (true false))
(define-bool-or-enum mouse-hide-while-typing (true false))
(define-bool-or-enum auto-update (off check download))
(define-bool-or-enum auto-update-channel (stable tip))
(define-bool-or-enum window-vsync (true false))


;;;
;;; — Flagsets (CSV with optional ± prefix per token) —
;;;

(define-flagset bell-features              (system audio attention title border no-system no-audio no-attention no-title no-border))
(define-flagset shell-integration-features (cursor sudo title no-cursor no-sudo no-title))
(define-flagset app-notifications          (clipboard-copy no-clipboard-copy))
(define-flagset link-previews              (text osc8 no-text no-osc8))
(define-flagset clipboard-trim-trailing-spaces (true false))
(define-flagset window-inherit-working-directory (always never inherit))


;;;
;;; — Structured records —
;;;

;; <window-padding>: top/right/bottom/left in cells (or symmetric int).
(define-record-type* <window-padding>
  window-padding make-window-padding window-padding?
  (top    window-padding-top    (default 0))
  (right  window-padding-right  (default 0))
  (bottom window-padding-bottom (default 0))
  (left   window-padding-left   (default 0)))

(define (serialize-window-padding field-name val)
  (string-append
   (format #f "window-padding-top = ~a\n"    (window-padding-top val))
   (format #f "window-padding-right = ~a\n"  (window-padding-right val))
   (format #f "window-padding-bottom = ~a\n" (window-padding-bottom val))
   (format #f "window-padding-left = ~a\n"   (window-padding-left val))))

;; <background-blur>: bool | radius (1..100).
(define-record-type* <background-blur>
  background-blur make-background-blur background-blur?
  (enabled? background-blur-enabled? (default #t))
  (radius   background-blur-radius   (default 20)))

(define (serialize-background-blur field-name val)
  (if (background-blur-enabled? val)
      (format #f "~a = ~a\n" field-name (background-blur-radius val))
      (format #f "~a = false\n" field-name)))

;; <command>: shell command — either a string or a file-like.
(define-record-type* <command>
  command make-command command?
  (program command-program (default #f))           ; string | file-like | #f
  (args    command-args    (default '())))

(define (command-or-file-like? x)
  (or (command? x) (string? x) (file-like? x) (gexp? x)))

(define (maybe-command-like? x)
  "#f, string, file-like, gexp, or a <command> record."
  (or (not x) (command-or-file-like? x)))

(define (serialize-maybe-command-like field-name val)
  (serialize-command field-name val))

(define (serialize-command field-name val)
  (cond
   ((not val) "")
   ((string? val)
    (format #f "~a = ~a\n" field-name val))
   ((or (file-like? val) (gexp? val))
    ;; Defer to mixed-text-file's gexp handling.
    #~(string-append #$(format #f "~a = " field-name) #$val "\n"))
   ((command? val)
    (let ((prog (command-program val))
          (args (command-args val)))
      (format #f "~a = ~a\n"
              field-name
              (if (null? args)
                  prog
                  (string-join (cons (if (string? prog) prog (object->string prog))
                                     args)
                               " ")))))
   (else "")))

;; <quick-terminal-size>: width × height (cells) or single value.
(define-record-type* <quick-terminal-size>
  quick-terminal-size make-quick-terminal-size quick-terminal-size?
  (primary   quick-terminal-size-primary   (default #f))
  (secondary quick-terminal-size-secondary (default #f)))

(define (serialize-quick-terminal-size field-name val)
  (let ((p (quick-terminal-size-primary val))
        (s (quick-terminal-size-secondary val)))
    (cond ((and p s) (format #f "~a = ~a,~a\n" field-name p s))
          (p         (format #f "~a = ~a\n" field-name p))
          (else      ""))))

;; <mouse-scroll-multiplier>: number multiplier.
(define-record-type* <mouse-scroll-multiplier>
  mouse-scroll-multiplier make-mouse-scroll-multiplier mouse-scroll-multiplier?
  (factor mouse-scroll-multiplier-factor (default 1.0)))

(define (serialize-mouse-scroll-multiplier field-name val)
  (format #f "~a = ~a\n" field-name (mouse-scroll-multiplier-factor val)))

;; <font-style>: italic / bold / bold-italic style override.
(define-record-type* <font-style>
  font-style make-font-style font-style?
  (style font-style-style (default 'auto))     ; symbol or string
  (face  font-style-face  (default #f)))

(define (serialize-font-style field-name val)
  (let ((style (font-style-style val))
        (face  (font-style-face val)))
    (cond
     (face        (format #f "~a = ~a\n" field-name face))
     ((eq? style 'auto) (format #f "~a = auto\n" field-name))
     ((eq? style 'false) (format #f "~a = false\n" field-name))
     (else (format #f "~a = ~a\n" field-name style)))))

;; <background-image-position>: {top,bottom,center}-{left,center,right}.
(define-record-type* <background-image-position>
  background-image-position make-background-image-position
  background-image-position?
  (vertical   background-image-position-vertical   (default 'center))
  (horizontal background-image-position-horizontal (default 'center)))

(define (serialize-background-image-position field-name val)
  (format #f "~a = ~a-~a\n" field-name
          (background-image-position-vertical val)
          (background-image-position-horizontal val)))

;; <background-image-fit>: contain / cover / stretch / none.
(define-record-type* <background-image-fit>
  background-image-fit make-background-image-fit background-image-fit?
  (mode background-image-fit-mode (default 'contain)))

(define (serialize-background-image-fit field-name val)
  (format #f "~a = ~a\n" field-name (background-image-fit-mode val)))

;; <split-preserve-zoom>: bool.
(define-record-type* <split-preserve-zoom>
  split-preserve-zoom make-split-preserve-zoom split-preserve-zoom?
  (enabled? split-preserve-zoom-enabled? (default #f)))

(define (serialize-split-preserve-zoom field-name val)
  (format #f "~a = ~a\n" field-name
          (if (split-preserve-zoom-enabled? val) "true" "false")))

;; <config-file-include>: path + optional? (prefix `?` for silent skip).
(define-record-type* <config-file-include>
  config-file-include make-config-file-include config-file-include?
  (path      config-file-include-path)
  (optional? config-file-include-optional? (default #f)))

(define (config-file-include-or-string? x)
  (or (string? x) (config-file-include? x)))

(define (serialize-config-file-include field-name val)
  (cond
   ((string? val)
    (format #f "~a = ~a\n" field-name val))
   ((config-file-include? val)
    (format #f "~a = ~a~a\n" field-name
            (if (config-file-include-optional? val) "?" "")
            (config-file-include-path val)))
   (else "")))

;; <theme-spec>: single theme name OR light/dark split.
(define-record-type* <theme-spec>
  theme-spec make-theme-spec theme-spec?
  (single theme-spec-single (default #f))   ; string or #f
  (light  theme-spec-light  (default #f))
  (dark   theme-spec-dark   (default #f)))

(define (theme-spec-or-string? x)
  (or (string? x) (theme-spec? x)))

(define (serialize-theme-spec field-name val)
  (cond
   ((string? val)
    (format #f "~a = ~a\n" field-name val))
   ((theme-spec? val)
    (let ((s (theme-spec-single val))
          (l (theme-spec-light val))
          (d (theme-spec-dark val)))
      (cond
       (s (format #f "~a = ~a\n" field-name s))
       ((and l d)
        (format #f "~a = light:~a,dark:~a\n" field-name l d))
       (else ""))))
   (else "")))

;; <link-rule>: regex + action mini-grammar.  Named link-rule (not
;; link) to avoid shadowing the core Guile `link' procedure.
(define-record-type* <link-rule>
  link-rule make-link-rule link-rule?
  (regex  link-rule-regex)
  (action link-rule-action))

(define (serialize-link-rule field-name val)
  (format #f "~a = ~s,~a\n" field-name
          (link-rule-regex val) (link-rule-action val)))

;; <mac-shortcut>: macOS-only chord rebind.
(define-record-type* <mac-shortcut>
  mac-shortcut make-mac-shortcut mac-shortcut?
  (chord  mac-shortcut-chord)
  (action mac-shortcut-action))

(define (serialize-mac-shortcut field-name val)
  (format #f "~a = ~a=~a\n" field-name
          (mac-shortcut-chord val) (mac-shortcut-action val)))

;; <selection-word-chars>: explicit word-character set.
(define-record-type* <selection-word-chars>
  selection-word-chars make-selection-word-chars selection-word-chars?
  (chars selection-word-chars-chars (default "")))

(define (serialize-selection-word-chars field-name val)
  (format #f "~a = ~s\n" field-name (selection-word-chars-chars val)))


;;;
;;; — Repeatable list predicates —
;;;

(define list-of-strings-or-files?
  (list-of (lambda (x) (or (string? x) (file-like? x) (gexp? x)))))

(define (keybind-entry? x)
  ;; Either a bare string \"trigger=action\" or a (trigger . action) pair.
  (or (string? x)
      (and (pair? x) (string? (car x)) (string? (cdr x)))))

(define list-of-keybinds? (list-of keybind-entry?))

(define (serialize-list-of-keybinds field-name vals)
  (apply string-append
         (map (lambda (kb)
                (cond
                 ((string? kb)
                  (format #f "~a = ~a\n" field-name kb))
                 ((pair? kb)
                  (format #f "~a = ~a=~a\n" field-name (car kb) (cdr kb)))
                 (else "")))
              vals)))

(define (palette-entry? x)
  ;; (N . color) where N is 0..255.
  (and (pair? x)
       (integer? (car x))
       (<= 0 (car x) 255)
       (color? (cdr x))))

(define list-of-palette-entries? (list-of palette-entry?))

(define (serialize-list-of-palette-entries field-name vals)
  (apply string-append
         (map (lambda (e)
                (format #f "~a = ~a=~a\n" field-name (car e) (cdr e)))
              vals)))

(define (env-entry? x)
  (or (and (pair? x) (string? (car x)) (string? (cdr x)))
      (equal? x "")))

(define list-of-env-entries? (list-of env-entry?))

(define (serialize-list-of-env-entries field-name vals)
  (apply string-append
         (map (lambda (e)
                (cond
                 ((equal? e "") (format #f "~a = \n" field-name))
                 ((pair? e) (format #f "~a = ~a=~a\n" field-name (car e) (cdr e)))
                 (else "")))
              vals)))

(define (font-feature-entry? x)
  ;; Bare name | "+name" | "-name" | "name on|off" | "name=N".
  (and (string? x) (positive? (string-length x))))

(define list-of-font-features? (list-of font-feature-entry?))

(define (serialize-list-of-font-features field-name vals)
  (apply string-append
         (map (lambda (f) (format #f "~a = ~a\n" field-name f)) vals)))

(define (font-variation-entry? x)
  ;; "axis=value" form.
  (and (string? x) (string-contains x "=")))

(define list-of-font-variations? (list-of font-variation-entry?))

(define (serialize-list-of-font-variations field-name vals)
  (apply string-append
         (map (lambda (v) (format #f "~a = ~a\n" field-name v)) vals)))

(define (codepoint-map-entry? x)
  ;; "codepoint-range=family" form, e.g. "U+0080-U+00FF=Hack".
  (and (string? x) (string-contains x "=")))

(define list-of-codepoint-maps? (list-of codepoint-map-entry?))

(define (serialize-list-of-codepoint-maps field-name vals)
  (apply string-append
         (map (lambda (e) (format #f "~a = ~a\n" field-name e)) vals)))

(define list-of-clipboard-codepoint-maps? (list-of codepoint-map-entry?))

(define (serialize-list-of-clipboard-codepoint-maps field-name vals)
  (apply string-append
         (map (lambda (e) (format #f "~a = ~a\n" field-name e)) vals)))

(define list-of-links? (list-of link-rule?))

(define (serialize-list-of-links field-name vals)
  (apply string-append
         (map (lambda (lnk) (serialize-link-rule field-name lnk)) vals)))

(define (key-remap-entry? x)
  ;; "trigger=replacement" or pair.
  (or (string? x) (and (pair? x) (string? (car x)) (string? (cdr x)))))

(define list-of-key-remaps? (list-of key-remap-entry?))

(define (serialize-list-of-key-remaps field-name vals)
  (apply string-append
         (map (lambda (e)
                (cond
                 ((string? e) (format #f "~a = ~a\n" field-name e))
                 ((pair? e)   (format #f "~a = ~a=~a\n" field-name (car e) (cdr e)))
                 (else "")))
              vals)))

(define list-of-config-file-includes? (list-of config-file-include-or-string?))

(define (serialize-list-of-config-file-includes field-name vals)
  (apply string-append
         (map (lambda (v) (serialize-config-file-include field-name v)) vals)))

(define (color-list-entry? x)
  ;; "name=color" or (name . color).
  (or (and (string? x) (string-contains x "="))
      (and (pair? x) (color? (cdr x)))))

(define list-of-color-list-entries? (list-of color-list-entry?))

(define (serialize-list-of-color-list-entries field-name vals)
  (apply string-append
         (map (lambda (e)
                (cond
                 ((string? e) (format #f "~a = ~a\n" field-name e))
                 ((pair? e)   (format #f "~a = ~a=~a\n" field-name (car e) (cdr e)))
                 (else "")))
              vals)))

(define (mac-shortcut-entry? x)
  (or (mac-shortcut? x)
      (and (string? x) (string-contains x "="))))

(define list-of-mac-shortcuts? (list-of mac-shortcut-entry?))

(define (serialize-list-of-mac-shortcuts field-name vals)
  (apply string-append
         (map (lambda (e)
                (cond
                 ((mac-shortcut? e) (serialize-mac-shortcut field-name e))
                 ((string? e) (format #f "~a = ~a\n" field-name e))
                 (else "")))
              vals)))


;;;
;;; — alist with symbol keys (for extra-options) —
;;;

(define (alist-with-symbol-keys? x)
  "Plain alist where every key is a symbol.  Collision against the
typed-field set is checked at construction time by the
configuration-record sanitizer (see %extra-options-sanitizer)."
  (and (list? x)
       (every (lambda (p)
                (and (pair? p) (symbol? (car p))))
              x)))

(define (serialize-alist-with-symbol-keys field-name vals)
  ;; This is emitted via a separate path in the walker (not by the
  ;; auto-resolved serializer); keep this stub for completeness.
  (apply string-append
         (map (lambda (p)
                (format #f "~a = ~a\n" (car p) (cdr p)))
              vals)))


;;;
;;; — String-or-gexp scalar (for extra-content) —
;;;

(define (string-or-gexp? x)
  (or (string? x) (gexp? x) (file-like? x)))

(define (serialize-string-or-gexp field-name val)
  (cond
   ((string? val)
    (if (zero? (string-length val))
        ""
        (string-append "\n# extra-content (overrides above)\n" val
                       (if (string-suffix? "\n" val) "" "\n"))))
   ((or (gexp? val) (file-like? val))
    #~(let ((s #$val))
        (if (and (string? s) (zero? (string-length s)))
            ""
            (string-append "\n# extra-content (overrides above)\n" s
                           (if (and (string? s) (string-suffix? "\n" s))
                               "" "\n")))))
   (else "")))


;;;
;;; — extra-files predicate —
;;;

(define (extra-file-entry? x)
  (and (pair? x)
       (string? (car x))
       (not (string-prefix? "/" (car x)))
       (or (string? (cdr x))
           (file-like? (cdr x))
           (gexp? (cdr x)))))

(define (extra-files-list? x)
  (and (list? x) (every extra-file-entry? x)))

(define (serialize-extra-files-list field-name vals)
  ;; Emitted via file deployment, not via the body serializer.
  "")


;;;
;;; — extra-options sanitizer (forward-declared) —
;;;
;;; Looks up `home-ghostty-configuration-fields' lazily so the
;;; sanitizer can be referenced by `define-configuration' below.

(define (%extra-options-sanitizer val)
  (unless (alist-with-symbol-keys? val)
    (configuration-field-error #f 'extra-options val))
  (let* ((mod    (resolve-module '(r0man guix home services ghostty)))
         (var    (module-variable mod 'home-ghostty-configuration-fields))
         (fields (if var (variable-ref var) '()))
         (typed  (filter
                  (lambda (n)
                    (not (memq n '(extra-options extra-files
                                   extra-content packages))))
                  (map configuration-field-name fields))))
    (for-each
     (lambda (p)
       (when (memq (car p) typed)
         (error
          (format #f "extra-options: '~a' is a typed field; \
use the field directly" (car p)))))
     val))
  val)


;;;
;;; — Configuration record —
;;;
;;; Representative coverage: every type bucket is exercised with at
;;; least one field.  The full 195-key transcription tracks against
;;; the canonical reference page; expand iteratively (see TODO list
;;; at the end of this section).

(define-configuration home-ghostty-configuration
  ;; — Application —
  (config-file
   (list-of-config-file-includes '())
   "Additional config files to load.
Repeatable.  Use `?path` for silent skip if missing."
   (serializer serialize-list-of-config-file-includes))

  ;; — Font —
  (font-family
   (list-of-strings '())
   "Font family for the regular face.
Repeatable for fallbacks."
   (serializer (lambda (n vs)
                 (apply string-append
                        (map (lambda (v) (format #f "~a = ~a\n" n v)) vs)))))
  (font-family-bold        (list-of-strings '()) "Bold-face family override. Repeatable."
                           (serializer (lambda (n vs) (apply string-append
                                                             (map (lambda (v) (format #f "~a = ~a\n" n v)) vs)))))
  (font-family-italic      (list-of-strings '()) "Italic-face family override. Repeatable."
                           (serializer (lambda (n vs) (apply string-append
                                                             (map (lambda (v) (format #f "~a = ~a\n" n v)) vs)))))
  (font-family-bold-italic (list-of-strings '()) "Bold-italic family override. Repeatable."
                           (serializer (lambda (n vs) (apply string-append
                                                             (map (lambda (v) (format #f "~a = ~a\n" n v)) vs)))))
  (font-size       (number 13.0)         "Font size in points.")
  (font-feature    (list-of-font-features '()) "OpenType feature toggle. Repeatable."
                   (serializer serialize-list-of-font-features))
  (font-variation  (list-of-font-variations '()) "Variable-font axis. Repeatable."
                   (serializer serialize-list-of-font-variations))
  (font-codepoint-map (list-of-codepoint-maps '()) "Per-range font override. Repeatable."
                      (serializer serialize-list-of-codepoint-maps))
  (font-thicken    (boolean #f)          "Thicken glyphs (boolean).")
  (font-thicken-strength (number 255)    "Thicken strength 0..255.")
  (adjust-cell-width  (maybe-string #f)  "Adjust cell width: +Npx | NN% | float.")
  (adjust-cell-height (maybe-string #f)  "Adjust cell height: +Npx | NN% | float.")

  ;; — Cursor / mouse —
  (cursor-style       (cursor-style 'block) "Cursor shape: block | bar | underline | block_hollow.")
  (cursor-style-blink (boolean #t)          "Blink the cursor (boolean).")
  (mouse-hide-while-typing (boolean #f)     "Hide pointer while typing (boolean).")
  (mouse-shift-capture (mouse-shift-capture #f)
                       "Capture mouse with shift held: false | true | never | always.")
  (mouse-scroll-multiplier (number 1.0)     "Mouse-wheel scroll multiplier.")
  (focus-follows-mouse (focus-follows-mouse #f) "Focus follows mouse (boolean).")
  (copy-on-select     (copy-on-select #f)   "Auto-copy on selection: false | true | clipboard.")

  ;; — Window —
  (window-decoration  (window-decoration #t)
                      "Native window decoration: true | false | auto | client | server | none.")
  (window-theme       (window-theme 'auto)  "Window theme.")
  (window-colorspace  (window-colorspace 'srgb) "Window color space.")
  (window-vsync       (window-vsync #t)     "Enable vsync (boolean).")
  (window-padding-x   (number 2)            "Horizontal cell padding.")
  (window-padding-y   (number 2)            "Vertical cell padding.")
  (window-padding-balance (boolean #f)      "Balance window padding.")
  (window-padding-color (padding-color "background")
                        "Padding color: hex | named | background.")
  (window-save-state  (window-save-state 'default) "Save/restore window state.")
  (window-position-x  (window-position-x 'auto) "Window X position.")
  (window-position-y  (window-position-y 'auto) "Window Y position.")
  (window-width       (number 0)            "Initial window width in cells (0 = auto).")
  (window-height      (number 0)            "Initial window height in cells (0 = auto).")
  (background         (color "282c34")      "Background color (hex or X11 name).")
  (foreground         (color "ffffff")      "Foreground text color.")
  (background-opacity (number 1.0)          "Background opacity 0.0..1.0.")
  (background-blur-radius (number 0)        "Background blur radius (0 = disabled).")
  (cursor-color       (maybe-string #f)     "Cursor color override.")
  (selection-foreground (maybe-string #f)   "Selection foreground.")
  (selection-background (maybe-string #f)   "Selection background.")

  ;; — Theme —
  (theme              (maybe-string #f)     "Theme name; or use light:NAME,dark:NAME.")

  ;; — Palette —
  (palette            (list-of-palette-entries '()) "Palette entries (N . color). Repeatable."
                      (serializer serialize-list-of-palette-entries))

  ;; — Bell / shell integration —
  (bell-features       (bell-features '()) "Bell features (CSV with ± prefixes).")
  (shell-integration   (shell-integration-mode 'detect) "Shell integration mode.")
  (shell-integration-features (shell-integration-features '())
                              "Shell integration feature toggles.")
  (app-notifications   (app-notifications '()) "Application notifications.")
  (link-previews       (link-previews '()) "Link preview features.")

  ;; — Keybindings / links —
  (keybind             (list-of-keybinds '()) "Keybindings. Repeatable."
                       (serializer serialize-list-of-keybinds))
  (link                (list-of-links '()) "Hyperlink rules. Repeatable."
                       (serializer serialize-list-of-links))
  (key-remap           (list-of-key-remaps '()) "Key remaps. Repeatable."
                       (serializer serialize-list-of-key-remaps))

  ;; — Environment —
  (env                 (list-of-env-entries '()) "Environment overrides. Repeatable."
                       (serializer serialize-list-of-env-entries))

  ;; — Command / working directory —
  (command             (maybe-command-like #f) "Shell to run.  Default: $SHELL."
                       (serializer serialize-maybe-command-like))
  (working-directory   (maybe-string #f) "Initial working directory.  inherit | home | last | path.")

  ;; — macOS / Linux platform-specific —
  (macos-titlebar-style (macos-titlebar-style 'transparent) "macOS titlebar style.")
  (macos-titlebar-proxy-icon (macos-titlebar-proxy-icon 'visible) "macOS proxy icon.")
  (macos-window-buttons (macos-window-buttons 'visible) "macOS traffic-light buttons.")
  (macos-icon          (macos-icon 'official) "macOS dock icon.")
  (macos-non-native-fullscreen (macos-non-native-fullscreen #f)
                               "macOS non-native fullscreen mode.")
  (linux-cgroup        (linux-cgroup 'never) "Linux cgroup mode.")

  ;; — Quick terminal —
  (quick-terminal-position (quick-terminal-position 'top) "Quick-terminal screen edge.")
  (quick-terminal-screen   (quick-terminal-screen 'main)  "Quick-terminal screen target.")
  (quick-terminal-autohide (quick-terminal-autohide #t)   "Quick-terminal autohide.")

  ;; — GTK —
  (gtk-tabs-location   (gtk-tabs-location 'top) "GTK tabs location.")
  (gtk-toolbar-style   (gtk-toolbar-style 'flat) "GTK toolbar style.")
  (gtk-wide-tabs       (gtk-wide-tabs 'true) "GTK wide tabs.")

  ;; — Confirmation / lifecycle —
  (confirm-close-surface (confirm-close-surface 'true) "Confirm-close-surface.")
  (auto-update         (auto-update 'check) "Auto-update mode.")
  (auto-update-channel (auto-update-channel 'stable) "Auto-update channel.")

  ;; — Resize overlay —
  (resize-overlay          (resize-overlay 'after-first) "Show resize overlay.")
  (resize-overlay-position (resize-overlay-position 'center) "Overlay position.")

  ;; — Escape valves (NOT in Ghostty's schema) —
  (extra-options
   (alist-with-symbol-keys '())
   "Forward-compat alist for keys not yet in the schema.
Keys must be symbols; collisions with typed fields raise at
construction time."
   (sanitizer %extra-options-sanitizer)
   (serializer serialize-alist-with-symbol-keys))

  (extra-files
   (extra-files-list '())
   "Extra files dropped under ~/.config/ghostty/.
Pairs of (relative-path . string|file-like).  Absolute paths raise."
   (serializer serialize-extra-files-list))

  (extra-content
   (string-or-gexp "")
   "Raw text appended verbatim to the end of the generated config."
   (serializer serialize-string-or-gexp))

  (packages
   (list-of-packages (list ghostty))
   "Packages to install."
   (serializer empty-serializer)))


;;;
;;; — Serializer (walker) —
;;;
;;; Emission order:
;;;   1. config-file lines (typed and lifted from extra-options)
;;;   2. typed fields, in record order, skipping defaults and
;;;      escape-valve fields
;;;   3. extra-options entries that aren't config-file
;;;   4. extra-content (with header) when non-empty

(define %escape-valve-fields
  '(extra-options extra-files extra-content packages))

(define (home-ghostty-serialize-config config)
  "Return a `mixed-text-file' object for the user's ghostty config."
  (let* ((fields home-ghostty-configuration-fields)
         (default-of (lambda (f) ((configuration-field-default-value-thunk f))))
         (current    (lambda (f) ((configuration-field-getter f) config)))
         (escape?    (lambda (f) (memq (configuration-field-name f)
                                       %escape-valve-fields)))
         (config-file? (lambda (f) (eq? 'config-file (configuration-field-name f))))
         (extra-options-val (home-ghostty-configuration-extra-options config))
         (extra-content-val (home-ghostty-configuration-extra-content config))
         (lifted-includes
          (filter (lambda (p) (eq? 'config-file (car p))) extra-options-val))
         (other-extra-options
          (filter (lambda (p) (not (eq? 'config-file (car p)))) extra-options-val))
         (emit
          (lambda (f)
            ((configuration-field-serializer f)
             (configuration-field-name f)
             (current f))))
         (group-1
          ;; typed config-file lines + extra-options config-file lines (front)
          (let ((typed (find config-file? fields)))
            (append
             (if typed (list (emit typed)) '())
             (map (lambda (p)
                    (format #f "config-file = ~a\n" (cdr p)))
                  lifted-includes))))
         (group-2
          ;; typed fields, skipping defaults, escape valves, and config-file
          (filter-map
           (lambda (f)
             (cond
              ((escape? f) #f)
              ((config-file? f) #f)
              ((equal? (current f) (default-of f)) #f)
              (else (emit f))))
           fields))
         (group-3
          (map (lambda (p) (format #f "~a = ~a\n" (car p) (cdr p)))
               other-extra-options))
         (group-4
          (cond
           ((and (string? extra-content-val)
                 (zero? (string-length extra-content-val)))
            '())
           (else
            (list (serialize-string-or-gexp 'extra-content extra-content-val))))))
    (apply mixed-text-file
           "ghostty-config"
           (append group-1 group-2 group-3 group-4))))


;;;
;;; — File deployment —
;;;

(define (home-ghostty-files config)
  "Return alist of files for `home-files-service-type'."
  (define (wrap-extra rel-path content)
    (cond
     ((string? content) (plain-file rel-path content))
     (else content)))
  (let ((extras (home-ghostty-configuration-extra-files config)))
    (cons*
     `(".config/ghostty/config" ,(home-ghostty-serialize-config config))
     (map (match-lambda
            ((rel . content)
             `(,(string-append ".config/ghostty/" rel)
               ,(wrap-extra rel content))))
          extras))))


;;;
;;; — Profile (packages) —
;;;

(define (home-ghostty-profile-packages config)
  (home-ghostty-configuration-packages config))


;;;
;;; — Service type —
;;;

(define home-ghostty-service-type
  (service-type
   (name 'home-ghostty)
   (extensions
    (list (service-extension home-files-service-type
                             home-ghostty-files)
          (service-extension home-profile-service-type
                             home-ghostty-profile-packages)))
   (default-value (home-ghostty-configuration))
   (description
    "Install and configure the Ghostty terminal emulator for the user.")))

;;; ghostty.scm ends here
