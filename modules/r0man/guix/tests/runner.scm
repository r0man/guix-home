;;; Copyright © 2026 Roman Scherer <roman@burningswell.com>
;;;
;;; This file is part of the r0man Guix Home channel.

(define-module (r0man guix tests runner)
  #:use-module (gnu tests)
  #:use-module (guix derivations)
  #:use-module (guix monads)
  #:use-module (guix store)
  #:export (%run-system-tests?
            run-system-test))

;;; Commentary:
;;;
;;; Helpers for running r0man Guix Home VM-based system tests under the
;;; Automake/SRFI-64 harness.  Mirrors `(asahi guix tests runner)`:
;;; `%run-system-tests?` is the parameter that test files consult to
;;; decide whether to skip; it's seeded from the
;;; `SYSTEM_TESTS` environment variable, which the Makefile's
;;; `check-system` target sets.  `run-system-test` builds a system-test
;;; record's derivation in a fresh store connection and returns #t on
;;; success, #f on failure.
;;;
;;; Code:

(define (env-truthy? value)
  (and (string? value)
       (not (string-null? value))
       (not (member (string-downcase value) '("0" "no" "false")))))

(define %run-system-tests?
  (make-parameter (env-truthy? (getenv "SYSTEM_TESTS"))))

(define (run-system-test test)
  "Build TEST, a system-test record, in a fresh store connection.
Return #t on success, #f on failure."
  (with-store store
    (catch #t
      (lambda ()
        (let ((drv (run-with-store store (system-test-value test))))
          (build-derivations store (list drv))
          #t))
      (lambda (key . rest)
        (when (memq key '(quit signal))
          (apply throw key rest))
        #f))))

;;; runner.scm ends here
