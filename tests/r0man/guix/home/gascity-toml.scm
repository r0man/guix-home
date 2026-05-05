;;; Copyright © 2026 Roman Scherer <roman@burningswell.com>
;;;
;;; This file is part of the r0man Guix Home channel.

(define-module (tests r0man guix home gascity-toml)
  #:use-module (r0man guix home services gascity-toml)
  #:use-module (srfi srfi-64))

(define suite "test-r0man-home-gascity-toml")

(test-begin suite)

;; Unit tests for derive-beads-prefix, which mirrors gascity's
;; DeriveBeadsPrefix at internal/config/config.go:728-754.  The expected
;; outputs were verified against the upstream Go implementation by
;; running the algorithm directly; if upstream changes, both sides need
;; updating.
(let ((cases '(("" . "")
               ("x" . "x")
               ("foo" . "foo")
               ("foo-py" . "foo")
               ("foo-bar" . "fb")
               ("XMLParser" . "xm")
               ("4chan" . "4c")
               ("---___" . "--")
               ("foo-py-py" . "fp")
               ("GasCity" . "gc")
               ("myFrontend" . "mf")
               ("go-py" . "go")
               ("ab" . "ab")
               ("abc" . "abc")
               ("abcd" . "ab"))))
  (for-each
   (lambda (kv)
     (test-equal (string-append "derive-beads-prefix " (car kv))
       (cdr kv)
       (derive-beads-prefix (car kv))))
   cases))

;; Golden-file comparisons for the renderer — keep our [[rigs]] entries
;; matching what `gc rig add' would write itself, so its re-add fast-path
;; (cmd_rig.go:243-268) treats subsequent shepherd-time invocations as
;; no-op-on-toml.
(test-equal "render-supervisor-toml port = 9372"
  "[supervisor]\n  port = 9372\n"
  (render-supervisor-toml #:port 9372))

(test-equal "render-cities-toml two cities"
  (string-append
   "[[cities]]\n"
   "  path = \"/home/alice/cities/test2\"\n"
   "  name = \"test2\"\n"
   "\n"
   "[[cities]]\n"
   "  path = \"/home/alice/cities/other\"\n"
   "  name = \"other\"\n"
   "\n")
  (render-cities-toml
   `(((path . "/home/alice/cities/test2") (name . "test2"))
     ((path . "/home/alice/cities/other") (name . "other")))))

(test-equal "render-rigs-section uses derived prefix"
  (string-append
   "[[rigs]]\n"
   "  name = \"GasCity\"\n"
   "  path = \"/repos/GasCity\"\n"
   "  prefix = \"gc\"\n"
   "\n")
  (render-rigs-section
   `(((path   . "/repos/GasCity")
      (name   . "GasCity")
      (prefix . "gc")))))

(test-end suite)
