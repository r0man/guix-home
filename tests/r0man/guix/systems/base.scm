;;; Copyright © 2026 Roman Scherer <roman@burningswell.com>
;;;
;;; This file is part of the r0man Guix Home channel.

(define-module (tests r0man guix systems base)
  #:use-module (r0man guix tests runner)
  #:use-module (r0man guix tests systems base)
  #:use-module (srfi srfi-64))

(define suite "test-r0man-base-os")

(test-begin suite)

;; VM-based system tests boot a real QEMU VM and are slow; only run
;; when R0MAN_RUN_SYSTEM_TESTS is set (e.g. via `make check-system`).
(unless (%run-system-tests?)
  (test-skip 1))

(test-assert "r0man-base-os boots and passes marionette assertions"
  (run-system-test %test-r0man-base-os))

(test-end suite)
