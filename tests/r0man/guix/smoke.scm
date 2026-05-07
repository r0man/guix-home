;;; Smoke test: load every module in the channel and assert one known
;;; export per module is well-defined.  Catches typos, broken imports,
;;; and unused-binding regressions across the whole channel in one
;;; pass — far cheaper than a real `guix home build`.

(define-module (test-r0man-smoke)
  #:use-module (srfi srfi-64))

(test-begin "r0man-smoke")

(define-syntax-rule (test-load-module name module export)
  (test-assert name
    (let ((mod (resolve-interface 'module)))
      (and (module-ref mod 'export) #t))))

(test-load-module "channels"
                  (r0man guix channels)
                  asahi-channel)

(test-load-module "home-bash"
                  (r0man guix home bash)
                  home-bash-default-configuration)

(test-load-module "home-channels"
                  (r0man guix home channels)
                  home-channels-default-list)

(test-load-module "home-packages"
                  (r0man guix home packages)
                  packages-base)

(test-load-module "home-tmux"
                  (r0man guix home tmux)
                  home-tmux-services)

(test-load-module "home-services-emacs"
                  (r0man guix home services emacs)
                  home-emacs-service-type)

(test-load-module "home-services-ghostty"
                  (r0man guix home services ghostty)
                  home-ghostty-service-type)

(test-load-module "home-services-ghostty/predicate"
                  (r0man guix home services ghostty)
                  color?)

(test-load-module "home-services-ghostty/record"
                  (r0man guix home services ghostty)
                  window-padding?)

(test-load-module "home-services-git"
                  (r0man guix home services git)
                  home-git-service-type)

(test-load-module "system-base"
                  (r0man guix system base)
                  base-operating-system)

(test-load-module "system-services-keyboard"
                  (r0man guix system services keyboard)
                  %keyboard-layout)

(test-load-module "tests"
                  (r0man guix tests)
                  %r0man-simple-os)

(test-end "r0man-smoke")
