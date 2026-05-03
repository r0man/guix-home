;;; Copyright © 2026 Roman Scherer <roman@burningswell.com>
;;;
;;; This file is part of the r0man Guix Home channel.

(define-module (tests r0man guix home bash)
  #:use-module (r0man guix tests home bash)
  #:use-module (r0man guix tests runner)
  #:use-module (srfi srfi-64))

(define suite "test-r0man-home-bash")

(test-begin suite)

;; VM-based system tests boot a real QEMU VM and are slow; only run
;; when SYSTEM_TESTS is set (e.g. via `make check-system`).
(unless (%run-system-tests?)
  (test-skip 1))

(test-assert "home-bash boots and produces /home/alice/.bashrc"
  (run-system-test %test-home-bash))

(test-end suite)
