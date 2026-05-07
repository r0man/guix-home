;;; Copyright (C) 2026 Roman Scherer <roman@burningswell.com>
;;;
;;; SRFI-64 unit tests for `(r0man guix home services ghostty)'.
;;; Pure tests; no daemon, no VM — exercises predicates, structured
;;; records, configuration schema, and the serializer end-to-end.

(define-module (test-r0man-home-ghostty)
  #:use-module (gnu services configuration)
  #:use-module (guix gexp)
  #:use-module (r0man guix home services ghostty)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64))

(test-begin "r0man-home-ghostty")


;;;
;;; Helpers
;;;

(define %ghostty-mod
  (resolve-module '(r0man guix home services ghostty)))

(define (ref sym)
  "Look up SYM in the ghostty module without listing every symbol in
the test file's `#:use-module' clause."
  (module-ref %ghostty-mod sym))

(define (predicate-of name-sym)
  "Return the predicate matching NAME-SYM (e.g. 'cursor-style ->
cursor-style?)."
  (ref (string->symbol (string-append (symbol->string name-sym) "?"))))

(define (serializer-of name-sym)
  "Return the serializer for NAME-SYM (e.g. 'cursor-style ->
serialize-cursor-style)."
  (ref (string->symbol (string-append "serialize-" (symbol->string name-sym)))))


;;;
;;; Predicates
;;;

(test-group "predicates/color"
  (test-assert "RRGGBB hex"      (color? "ff0080"))
  (test-assert "#RRGGBB hex"     (color? "#ff0080"))
  (test-assert "X11 named"       (color? "rebeccapurple"))
  (test-assert "rejects #RGB"    (not (color? "#fff")))
  (test-assert "rejects symbol"  (not (color? 'red)))
  (test-assert "rejects empty"   (not (color? "")))
  (test-assert "rejects cell-foreground for color?"
    (not (color? "cell-foreground"))))

(test-group "predicates/cell-color"
  (test-assert "color? still works"        (cell-color? "ff0080"))
  (test-assert "accepts cell-foreground"   (cell-color? "cell-foreground"))
  (test-assert "accepts cell-background"   (cell-color? "cell-background"))
  (test-assert "rejects 'background"       (not (cell-color? 'background)))
  (test-assert "rejects symbol"            (not (cell-color? 'foo))))

(test-group "predicates/padding-color"
  (test-assert "color? still works"      (padding-color? "ff0080"))
  (test-assert "accepts \"background\""  (padding-color? "background"))
  (test-assert "rejects cell-foreground" (not (padding-color? "cell-foreground")))
  (test-assert "rejects empty"           (not (padding-color? ""))))

(test-group "predicates/duration-string"
  (test-assert "1h30m"           (duration-string? "1h30m"))
  (test-assert "500ms"           (duration-string? "500ms"))
  (test-assert "30s"             (duration-string? "30s"))
  (test-assert "rejects no unit" (not (duration-string? "1234")))
  (test-assert "rejects empty"   (not (duration-string? "")))
  (test-assert "rejects non-string" (not (duration-string? 30))))

(test-group "predicates/percentage-string"
  (test-assert "30%"               (percentage-string? "30%"))
  (test-assert "12.5%"             (percentage-string? "12.5%"))
  (test-assert "rejects no %"      (not (percentage-string? "30")))
  (test-assert "rejects empty"     (not (percentage-string? "")))
  (test-assert "rejects symbol"    (not (percentage-string? '%))))

(test-group "predicates/metric-modifier"
  (test-assert "number"          (metric-modifier? 1.0))
  (test-assert "+2px"            (metric-modifier? "+2px"))
  (test-assert "-1px"            (metric-modifier? "-1px"))
  (test-assert "110%"            (metric-modifier? "110%"))
  (test-assert "rejects bare px" (not (metric-modifier? "2px")))
  (test-assert "rejects bare string" (not (metric-modifier? "foo"))))

(test-group "predicates/maybe-string"
  (test-assert "#f is OK"        (maybe-string? #f))
  (test-assert "string is OK"    (maybe-string? "x"))
  (test-assert "rejects symbol"  (not (maybe-string? 'x)))
  (test-assert "rejects number"  (not (maybe-string? 42))))


;;;
;;; Structured records
;;;

(test-group "records/window-padding"
  (let ((wp (window-padding (top 2) (right 4) (bottom 2) (left 4))))
    (test-assert "constructed"      (window-padding? wp))
    (test-assert "rejects symbol"   (not (window-padding? 'wp)))))

(test-group "records/config-file-include"
  (let* ((req  (config-file-include (path "themes/nord")))
         (opt  (config-file-include (path "themes/nord") (optional? #t))))
    (test-assert "required" (config-file-include? req))
    (test-assert "optional" (config-file-include? opt))))

(test-group "records/theme-spec"
  (let ((s (theme-spec (single "nord")))
        (d (theme-spec (light "tomorrow") (dark "nord"))))
    (test-assert "single" (theme-spec? s))
    (test-assert "split"  (theme-spec? d))))

(test-group "records/link-rule"
  (test-assert "constructed"
    (link-rule? (link-rule (regex "https?://[^ ]+") (action "open"))))
  (test-assert "rejects record-of-wrong-type"
    (not (link-rule? (theme-spec (single "x"))))))


;;;
;;; List-of predicates
;;;

(test-group "list-predicates/keybinds"
  (test-assert "string entry"  (list-of-keybinds? '("ctrl+b>c=close_tab")))
  (test-assert "pair entry"    (list-of-keybinds? '(("ctrl+a" . "reset"))))
  (test-assert "mixed"
    (list-of-keybinds? '(("ctrl+a" . "reset") "ctrl+b>c=close_tab")))
  (test-assert "rejects non-list" (not (list-of-keybinds? "ctrl+a"))))

(test-group "list-predicates/palette"
  (test-assert "valid"   (list-of-palette-entries? '((0 . "000000") (1 . "ff0000"))))
  (test-assert "rejects out-of-range"
    (not (list-of-palette-entries? '((300 . "000000")))))
  (test-assert "rejects non-color"
    (not (list-of-palette-entries? '((0 . 'oops))))))

(test-group "list-predicates/env"
  (test-assert "pair entries"
    (list-of-env-entries? '(("PATH" . "/bin") ("FOO" . "bar"))))
  (test-assert "reset entry"
    (list-of-env-entries? '("" ("FOO" . "bar")))))

(test-group "list-predicates/font-features"
  (test-assert "bare"  (list-of-font-features? '("ss01")))
  (test-assert "+name" (list-of-font-features? '("+liga")))
  (test-assert "name=N" (list-of-font-features? '("ss01=2"))))

(test-group "list-predicates/config-file-includes"
  (test-assert "string"
    (list-of-config-file-includes? '("themes/nord")))
  (test-assert "record"
    (list-of-config-file-includes?
     (list (config-file-include (path "themes/nord") (optional? #t))))))


;;;
;;; Enum data-table
;;;
;;; Each row: (name good-member bad-member).  All 49 closed enums.

(define %enum-table
  '((adjust-cursor-style                   block          __bogus__)
    (alpha-blending                        native         __bogus__)
    (async-backend                         epoll          __bogus__)
    (clipboard-paste-protection            true           __bogus__)
    (clipboard-rw-policy                   ask            __bogus__)
    (command-finder                        full-path      __bogus__)
    (confirm-close-surface                 always         __bogus__)
    (cursor-click-to-move                  true           __bogus__)
    (cursor-style                          bar            __bogus__)
    (fullscreen                            non-native     __bogus__)
    (grapheme-width-method                 unicode        __bogus__)
    (gtk-quick-terminal-layer              top            __bogus__)
    (gtk-tabs-location                     hidden         __bogus__)
    (gtk-titlebar-style                    tabs           __bogus__)
    (gtk-toolbar-style                     flat           __bogus__)
    (gtk-wide-tabs                         true           __bogus__)
    (keybind-action-policy                 prompt         __bogus__)
    (linux-cgroup                          single-instance __bogus__)
    (macos-dock-drop-behavior              new-tab        __bogus__)
    (macos-icon                            chalkboard     __bogus__)
    (macos-icon-frame                      chrome         __bogus__)
    (macos-shortcuts                       deny           __bogus__)
    (macos-titlebar-proxy-icon             hidden         __bogus__)
    (macos-titlebar-style                  transparent    __bogus__)
    (macos-window-buttons                  visible        __bogus__)
    (mouse-shift-capture-mode              always         __bogus__)
    (notify-on-command-finish-mode         unfocused      __bogus__)
    (osc-color-report-format               16-bit         __bogus__)
    (quick-terminal-animation-duration-mode manual        __bogus__)
    (quick-terminal-keyboard-interactivity on-demand      __bogus__)
    (quick-terminal-position               center         __bogus__)
    (quick-terminal-screen                 macos-menu-bar __bogus__)
    (quick-terminal-space-behavior         remain         __bogus__)
    (resize-overlay                        after-first    __bogus__)
    (resize-overlay-position               top-left       __bogus__)
    (right-click-action                    paste          __bogus__)
    (scrollbar                             auto           __bogus__)
    (shell-integration-mode                fish           __bogus__)
    (split-divider-color-policy            always-show    __bogus__)
    (tabs-on-bottom                        true           __bogus__)
    (unfocused-split-fill                  none           __bogus__)
    (window-colorspace                     display-p3     __bogus__)
    (window-decoration-mode                client         __bogus__)
    (window-new-tab-position               end            __bogus__)
    (window-position-x                     center         __bogus__)
    (window-position-y                     center         __bogus__)
    (window-save-state                     always         __bogus__)
    (window-show-tab-bar                   never          __bogus__)
    (window-theme                          glass          __bogus__)))

(test-group "enums/positive"
  (for-each
   (lambda (row)
     (let ((name (car row)) (good (cadr row)))
       (test-assert (symbol->string name)
         ((predicate-of name) good))))
   %enum-table))

(test-group "enums/negative"
  (for-each
   (lambda (row)
     (let ((name (car row)) (bad (caddr row)))
       (test-assert (symbol->string name)
         (not ((predicate-of name) bad)))))
   %enum-table))

(test-group "enums/serialize"
  (for-each
   (lambda (row)
     (let* ((name (car row)) (good (cadr row))
            (line ((serializer-of name) 'field good)))
       (test-equal (symbol->string name)
         (format #f "field = ~a\n" good)
         line)))
   %enum-table))


;;;
;;; Hybrid (bool-or-enum) data-table
;;;
;;; Each row: (name extension-symbol bogus-symbol).  All 15 hybrids.
;;; The booleans #t/#f are tested implicitly for every row.

(define %hybrid-table
  '((macos-non-native-fullscreen           visible-menu      __bogus__)
    (window-decoration                     client            __bogus__)
    (mouse-shift-capture                   never             __bogus__)
    (copy-on-select                        clipboard         __bogus__)
    (confirm-close-surface-mode            always            __bogus__)
    (gtk-single-instance                   desktop           __bogus__)
    (macos-option-as-alt                   left              __bogus__)
    (quick-terminal-autohide               focused           __bogus__)
    (focus-follows-mouse                   true              __bogus__)
    (mouse-hide-while-typing               true              __bogus__)
    (auto-update                           check             __bogus__)
    (auto-update-channel                   stable            __bogus__)
    (title-report-mode                     full              __bogus__)
    (window-subtitle-mode                  working-directory __bogus__)
    (window-vsync                          true              __bogus__)))

(test-group "hybrids/positive"
  (for-each
   (lambda (row)
     (let ((name (car row)) (ext (cadr row)))
       (let ((pred (predicate-of name)))
         (test-assert (string-append (symbol->string name) "/#t") (pred #t))
         (test-assert (string-append (symbol->string name) "/#f") (pred #f))
         (test-assert (string-append (symbol->string name) "/extension")
                      (pred ext)))))
   %hybrid-table))

(test-group "hybrids/negative"
  (for-each
   (lambda (row)
     (let ((name (car row)) (bad (caddr row)))
       (test-assert (symbol->string name)
         (not ((predicate-of name) bad)))))
   %hybrid-table))

(test-group "hybrids/serialize"
  (for-each
   (lambda (row)
     (let* ((name (car row)) (ext (cadr row))
            (ser  (serializer-of name)))
       (test-equal (string-append (symbol->string name) "/#t")
         "field = true\n"
         (ser 'field #t))
       (test-equal (string-append (symbol->string name) "/#f")
         "field = false\n"
         (ser 'field #f))
       (test-equal (string-append (symbol->string name) "/extension")
         (format #f "field = ~a\n" ext)
         (ser 'field ext))))
   %hybrid-table))


;;;
;;; Flagset data-table
;;;
;;; Each row: (name good-bare-symbol other-flagset-bogus-member).
;;; All 11 flagsets.

(define %flagset-table
  '((bell-features                          system    __bogus__)
    (shell-integration-features             cursor    __bogus__)
    (app-notifications                      clipboard-copy __bogus__)
    (link-previews                          text      __bogus__)
    (clipboard-trim-trailing-spaces         true      __bogus__)
    (window-inherit-working-directory       inherit   __bogus__)
    (font-synthetic-style                   bold      __bogus__)
    (font-shaping-break                     cgj       __bogus__)
    (freetype-load-flags                    hinting   __bogus__)
    (notify-on-command-finish-action        bell      __bogus__)
    (scroll-to-bottom                       keypress  __bogus__)))

(test-group "flagsets/positive"
  (for-each
   (lambda (row)
     (let* ((name (car row)) (bare (cadr row))
            (pred (predicate-of name))
            (plus  (string->symbol (string-append "+" (symbol->string bare))))
            (minus (string->symbol (string-append "-" (symbol->string bare)))))
       (test-assert (string-append (symbol->string name) "/bare")
                    (pred (list bare)))
       (test-assert (string-append (symbol->string name) "/+name")
                    (pred (list plus)))
       (test-assert (string-append (symbol->string name) "/-name")
                    (pred (list minus)))))
   %flagset-table))

(test-group "flagsets/negative"
  (for-each
   (lambda (row)
     (let ((name (car row)) (bad (caddr row)))
       (let ((pred (predicate-of name)))
         (test-assert (string-append (symbol->string name) "/sibling")
                      (not (pred (list bad))))
         (test-assert (string-append (symbol->string name) "/non-list")
                      (not (pred 'foo))))))
   %flagset-table))

(test-group "flagsets/serialize"
  (for-each
   (lambda (row)
     (let* ((name (car row)) (bare (cadr row))
            (ser  (serializer-of name)))
       (test-equal (symbol->string name)
         (format #f "field = ~a\n" bare)
         (ser 'field (list bare)))))
   %flagset-table))


;;;
;;; Per-structured-record serialization
;;;

(test-group "serialize/window-padding"
  (test-equal "all four sides"
    "window-padding-top = 2\nwindow-padding-right = 4\nwindow-padding-bottom = 2\nwindow-padding-left = 4\n"
    ((ref 'serialize-window-padding) 'window-padding
     (window-padding (top 2) (right 4) (bottom 2) (left 4)))))

(test-group "serialize/background-blur"
  (test-equal "enabled => radius"
    "background-blur = 30\n"
    ((ref 'serialize-background-blur) 'background-blur
     (background-blur (enabled? #t) (radius 30))))
  (test-equal "disabled => false"
    "background-blur = false\n"
    ((ref 'serialize-background-blur) 'background-blur
     (background-blur (enabled? #f)))))

(test-group "serialize/command"
  (test-equal "string only"
    "command = /bin/sh\n"
    ((ref 'serialize-command) 'command "/bin/sh"))
  (test-equal "with args"
    "command = bash --login\n"
    ((ref 'serialize-command) 'command
     (command (program "bash") (args '("--login"))))))

(test-group "serialize/quick-terminal-size"
  (test-equal "primary only"
    "quick-terminal-size = 25\n"
    ((ref 'serialize-quick-terminal-size) 'quick-terminal-size
     (quick-terminal-size (primary 25))))
  (test-equal "primary + secondary"
    "quick-terminal-size = 25,50\n"
    ((ref 'serialize-quick-terminal-size) 'quick-terminal-size
     (quick-terminal-size (primary 25) (secondary 50)))))

(test-group "serialize/mouse-scroll-multiplier"
  (test-equal "scalar"
    "mouse-scroll-multiplier = 1.5\n"
    ((ref 'serialize-mouse-scroll-multiplier) 'mouse-scroll-multiplier
     (mouse-scroll-multiplier (factor 1.5)))))

(test-group "serialize/font-style"
  (test-equal "auto"
    "font-style = auto\n"
    ((ref 'serialize-font-style) 'font-style (font-style (style 'auto))))
  (test-equal "false"
    "font-style = false\n"
    ((ref 'serialize-font-style) 'font-style (font-style (style 'false))))
  (test-equal "face"
    "font-style = Heavy\n"
    ((ref 'serialize-font-style) 'font-style (font-style (face "Heavy"))))
  (test-equal "string style"
    "font-style = Bold\n"
    ((ref 'serialize-font-style) 'font-style (font-style (style "Bold")))))

(test-group "serialize/background-image-position"
  (test-equal "vert-horiz"
    "background-image-position = top-left\n"
    ((ref 'serialize-background-image-position) 'background-image-position
     (background-image-position (vertical 'top) (horizontal 'left)))))

(test-group "serialize/background-image-fit"
  (test-equal "mode"
    "background-image-fit = cover\n"
    ((ref 'serialize-background-image-fit) 'background-image-fit
     (background-image-fit (mode 'cover)))))

(test-group "serialize/split-preserve-zoom"
  (test-equal "true"
    "split-preserve-zoom = true\n"
    ((ref 'serialize-split-preserve-zoom) 'split-preserve-zoom
     (split-preserve-zoom (enabled? #t))))
  (test-equal "false"
    "split-preserve-zoom = false\n"
    ((ref 'serialize-split-preserve-zoom) 'split-preserve-zoom
     (split-preserve-zoom (enabled? #f)))))

(test-group "serialize/config-file-include"
  (test-equal "required"
    "config-file = themes/nord\n"
    ((ref 'serialize-config-file-include) 'config-file
     (config-file-include (path "themes/nord"))))
  (test-equal "optional with ? prefix"
    "config-file = ?themes/nord\n"
    ((ref 'serialize-config-file-include) 'config-file
     (config-file-include (path "themes/nord") (optional? #t)))))

(test-group "serialize/theme-spec"
  (test-equal "single"
    "theme = nord\n"
    ((ref 'serialize-theme-spec) 'theme (theme-spec (single "nord"))))
  (test-equal "light + dark split"
    "theme = light:tomorrow,dark:nord\n"
    ((ref 'serialize-theme-spec) 'theme
     (theme-spec (light "tomorrow") (dark "nord")))))

(test-group "serialize/link-rule"
  (test-equal "regex,action"
    "link = \"https?://[^ ]+\",open\n"
    ((ref 'serialize-link-rule) 'link
     (link-rule (regex "https?://[^ ]+") (action "open")))))

(test-group "serialize/mac-shortcut"
  (test-equal "chord=action"
    "macos-shortcut = cmd+t=new_tab\n"
    ((ref 'serialize-mac-shortcut) 'macos-shortcut
     (mac-shortcut (chord "cmd+t") (action "new_tab")))))

(test-group "serialize/selection-word-chars"
  (test-equal "quoted form"
    "selection-word-chars = \"abc-_/\"\n"
    ((ref 'serialize-selection-word-chars) 'selection-word-chars
     (selection-word-chars (chars "abc-_/")))))


;;;
;;; Serializer-walker matrix
;;;

(test-group "walker/skip-if-default"
  (test-equal "default config => empty body"
    ""
    (home-ghostty-config->string (home-ghostty-configuration))))

(test-group "walker/set-one-field"
  (test-equal "single field => single line"
    "font-size = 16\n"
    (home-ghostty-config->string
     (home-ghostty-configuration (font-size 16)))))

(test-group "walker/config-file-ordering"
  ;; Typed config-file lines must precede other typed body lines.
  (let* ((s (home-ghostty-config->string
             (home-ghostty-configuration
              (config-file '("themes/nord"))
              (font-size 16))))
         (cf-pos (string-contains s "config-file = "))
         (fs-pos (string-contains s "font-size = ")))
    (test-assert "config-file appears before font-size"
      (and cf-pos fs-pos (< cf-pos fs-pos)))))

(test-group "walker/extra-options-config-file-lifting"
  ;; An extra-options config-file gets lifted into group-1 (front),
  ;; not group-3 (back).
  (let* ((s (home-ghostty-config->string
             (home-ghostty-configuration
              (font-size 16)
              (extra-options '((config-file . "themes/lifted"))))))
         (cf-pos (string-contains s "config-file = themes/lifted"))
         (fs-pos (string-contains s "font-size = ")))
    (test-assert "lifted config-file precedes font-size"
      (and cf-pos fs-pos (< cf-pos fs-pos)))))

(test-group "walker/extra-options-last-wins"
  ;; Both typed config-file lines and lifted lines appear; typed first,
  ;; lifted second (so the last one wins per Ghostty's last-wins rule).
  (let* ((s (home-ghostty-config->string
             (home-ghostty-configuration
              (config-file '("themes/typed"))
              (extra-options '((config-file . "themes/lifted"))))))
         (typed  (string-contains s "themes/typed"))
         (lifted (string-contains s "themes/lifted")))
    (test-assert "typed precedes lifted"
      (and typed lifted (< typed lifted)))))

(test-group "walker/repeatable"
  (test-equal "repeatable list field => N lines in input order"
    "font-family = A\nfont-family = B\n"
    (home-ghostty-config->string
     (home-ghostty-configuration (font-family '("A" "B"))))))

(test-group "walker/reset-semantics"
  ;; A "" entry in env emits a bare reset line, then subsequent pairs
  ;; emit normal lines.
  (test-equal "reset then pair"
    "env = \nenv = FOO=bar\n"
    (home-ghostty-config->string
     (home-ghostty-configuration (env '("" ("FOO" . "bar")))))))

(test-group "walker/structured-record-in-record"
  (test-equal "non-default background-image-position renders single line"
    "background-image-position = top-left\n"
    (home-ghostty-config->string
     (home-ghostty-configuration
      (background-image-position
       (background-image-position
        (vertical 'top) (horizontal 'left)))))))

(test-group "walker/extra-content-header"
  (test-equal "non-empty extra-content adds header"
    "\n# extra-content (overrides above)\nraw line\n"
    (home-ghostty-config->string
     (home-ghostty-configuration (extra-content "raw line"))))
  (test-equal "empty extra-content adds nothing"
    ""
    (home-ghostty-config->string
     (home-ghostty-configuration (extra-content "")))))

(test-group "walker/extra-options-non-config-file"
  ;; Non-config-file extra-options entries appear in group-3 (after
  ;; typed body lines).
  (let* ((s (home-ghostty-config->string
             (home-ghostty-configuration
              (font-size 16)
              (extra-options '((some-future-key . "value"))))))
         (fs (string-contains s "font-size = "))
         (xx (string-contains s "some-future-key = value")))
    (test-assert "extra-options follow typed lines"
      (and fs xx (< fs xx)))))

(test-group "walker/file-like-passthrough"
  (test-error "raises when command is a gexp"
    #t
    (home-ghostty-config->string
     (home-ghostty-configuration (command #~"/bin/foo")))))

(test-group "walker/idempotence"
  (let ((c (home-ghostty-configuration (font-size 14) (theme "nord"))))
    (test-equal "two calls => string=? results"
      (home-ghostty-config->string c)
      (home-ghostty-config->string c))))

(test-group "walker/order-stability"
  ;; Two configs differing only in record-arg input order render
  ;; identically (record fields drive output order).
  (test-equal "swapping construction args has no effect"
    (home-ghostty-config->string
     (home-ghostty-configuration (font-size 14) (theme "nord")))
    (home-ghostty-config->string
     (home-ghostty-configuration (theme "nord") (font-size 14)))))


;;;
;;; home-ghostty-files (file deployment)
;;;

(test-group "files/splice-shape"
  (let ((entries (home-ghostty-files (home-ghostty-configuration))))
    (test-assert "list of pairs"
      (and (list? entries) (every pair? entries)))
    (test-assert "every entry has .config/ghostty/ prefix"
      (every (lambda (e)
               (and (string? (car e))
                    (or (string=? (car e) ".config/ghostty/config")
                        (string-prefix? ".config/ghostty/" (car e)))))
             entries))))

(test-group "files/extra-files-string-wrap"
  ;; A string-valued extra-file gets wrapped in `plain-file'; the
  ;; resulting record satisfies `file-like?'.
  (let* ((entries (home-ghostty-files
                   (home-ghostty-configuration
                    (extra-files '(("themes/mine" . "background = ff0080\n"))))))
         (theme (assoc ".config/ghostty/themes/mine" entries)))
    (test-assert "themes/mine entry exists" theme)
    (test-assert "value satisfies file-like?"
      (file-like? (cadr theme)))))


;;;
;;; Configuration record / schema
;;;

(test-group "schema/default-construct"
  (test-assert "default config constructs"
    (home-ghostty-configuration? (home-ghostty-configuration))))

(test-group "schema/no-underscore-field-names"
  ;; Ghostty option names are documented as hyphenated.
  (test-assert "no '_' in any field name"
    (every (lambda (f)
             (not (string-contains
                   (symbol->string (configuration-field-name f)) "_")))
           home-ghostty-configuration-fields)))

(test-group "schema/has-fields"
  ;; Phase 3b: full transcription of the Ghostty docs reference page.
  ;; 202 typed fields (matching build-aux/scrape-ghostty-docs.scm) plus
  ;; 4 escape-valve fields: extra-options, extra-files, extra-content,
  ;; packages.
  (test-equal "schema has the full 202 typed fields + 4 escape valves"
    206
    (length home-ghostty-configuration-fields)))

(test-group "schema/extra-options-collision"
  ;; Constructing with extra-options that collides with a typed field
  ;; raises.
  (test-error "font-size collision raises"
    #t
    (home-ghostty-configuration
     (extra-options '((font-size . 16))))))

(test-group "schema/extra-options-non-collision"
  (test-assert "future-key extra-options accepted"
    (home-ghostty-configuration?
     (home-ghostty-configuration
      (extra-options '((some-future-key . "value")))))))

(test-group "schema/docstring-contract"
  ;; Every field's documentation must be <= 240 chars and <= 3 lines.
  ;; Naming the offending field on failure lets developers fix it
  ;; without grep-spelunking.
  (for-each
   (lambda (f)
     (let* ((name (configuration-field-name f))
            (doc  (configuration-field-documentation f))
            (lines (length (string-split doc #\newline))))
       (test-assert
           (format #f "~a: documentation is a string" name)
         (string? doc))
       (test-assert
           (format #f "~a: <=240 chars (got ~a)" name (string-length doc))
         (<= (string-length doc) 240))
       (test-assert
           (format #f "~a: <=3 lines (got ~a)" name lines)
         (<= lines 3))))
   home-ghostty-configuration-fields))


;;;
;;; Golden fixtures
;;;
;;; Two minimal/heavy fixtures live under tests/data/ghostty/.  Tests
;;; rebuild the same config in-process and compare against the on-disk
;;; bytes (header stripped).  Setting R0MAN_REGEN_GHOSTTY_GOLDEN=1
;;; rewrites the fixtures instead of asserting — the same path
;;; `make ghostty-golden' uses.

(define %golden-header
  "# Generated by 'make ghostty-golden' — DO NOT EDIT.\n")

(define %abs-top-srcdir
  (or (getenv "abs_top_srcdir")
      (and=> (getenv "top_srcdir")
             (lambda (s) (canonicalize-path s)))
      (and=> (current-filename)
             (lambda (f)
               (canonicalize-path
                (string-append (dirname f) "/../../../.."))))
      (canonicalize-path ".")))

(define (golden-path name)
  (string-append %abs-top-srcdir "/tests/data/ghostty/" name))

(define (regen?) (equal? (getenv "R0MAN_REGEN_GHOSTTY_GOLDEN") "1"))

(define (read-golden name)
  (call-with-input-file (golden-path name)
    (lambda (port)
      (let loop ((chars '()))
        (let ((c (read-char port)))
          (if (eof-object? c)
              (list->string (reverse chars))
              (loop (cons c chars))))))))

(define (write-golden name body)
  (call-with-output-file (golden-path name)
    (lambda (port)
      (display %golden-header port)
      (display body port))))

(define (strip-header text)
  (if (string-prefix? %golden-header text)
      (substring text (string-length %golden-header))
      text))

(define %golden-minimal-config
  (home-ghostty-configuration
   (font-size 14)
   (theme "nord")))

(define %golden-heavy-config
  (home-ghostty-configuration
   (config-file '("themes/nord"))
   (font-family '("Iosevka" "Hack"))
   (font-size 14)
   (font-feature '("+liga" "ss01"))
   (cursor-style 'bar)
   (background-blur (background-blur (enabled? #t) (radius 30)))
   (background-image-position
    (background-image-position (vertical 'top) (horizontal 'left)))
   (palette '((0 . "000000") (1 . "ff0000")))
   (env '("" ("FOO" . "bar")))
   (keybind '(("ctrl+a" . "reset") "ctrl+b>c=close_tab"))
   (bell-features '(audio +attention))
   (extra-options '((some-future-key . "future-value")))
   (extra-content "# user override\nfont-thicken = true\n")))

(test-group "golden/minimal"
  (let ((rendered (home-ghostty-config->string %golden-minimal-config)))
    (cond
     ((regen?)
      (write-golden "golden-minimal.config" rendered)
      (test-assert "regenerated" #t))
     (else
      (test-equal "matches golden-minimal.config"
        rendered
        (strip-header (read-golden "golden-minimal.config")))))))

(test-group "golden/heavy"
  (let ((rendered (home-ghostty-config->string %golden-heavy-config)))
    (cond
     ((regen?)
      (write-golden "golden-heavy.config" rendered)
      (test-assert "regenerated" #t))
     (else
      (test-equal "matches golden-heavy.config"
        rendered
        (strip-header (read-golden "golden-heavy.config")))))))

(test-end "r0man-home-ghostty")
