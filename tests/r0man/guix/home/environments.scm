;;; Per-environment loader test: resolves each home-environment module
;;; and asserts the exported home-environment is a `home-environment?`
;;; record.  This catches breakage in the dependency graph below each
;;; environment without paying for a `guix home build`.

(define-module (test-r0man-home-environments)
  #:use-module (gnu home)
  #:use-module (srfi srfi-64))

(test-begin "r0man-home-environments")

(define-syntax-rule (test-load-environment name module export)
  (test-assert name
    (let ((mod (resolve-interface 'module)))
      (home-environment? (module-ref mod 'export)))))

(test-load-environment "container"
                       (r0man guix home environments container)
                       container-home-environment)

(test-load-environment "gastown"
                       (r0man guix home environments gastown)
                       gastown-home-environment)

(test-load-environment "m1"
                       (r0man guix home environments m1)
                       m1-home-environment)

(test-load-environment "precision"
                       (r0man guix home environments precision)
                       precision-home-environment)

(test-load-environment "server"
                       (r0man guix home environments server)
                       server-home-environment)

(test-end "r0man-home-environments")
