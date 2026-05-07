;;; Copyright (C) 2026 Roman Scherer <roman@burningswell.com>
;;;
;;; SRFI-64 unit tests for `(r0man guix home services ghostty)'.
;;; Pure tests; no daemon, no VM — exercises predicates, structured
;;; records, configuration schema, and the serializer end-to-end.

(define-module (test-r0man-home-ghostty)
  #:use-module (gnu services configuration)
  #:use-module (guix gexp)
  #:use-module (r0man guix home services ghostty)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64))

(test-begin "r0man-home-ghostty")


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
  (test-assert "schema has at least 50 fields (representative subset)"
    (>= (length home-ghostty-configuration-fields) 50)))

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

(test-end "r0man-home-ghostty")
