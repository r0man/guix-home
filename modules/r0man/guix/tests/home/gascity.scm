;;; Copyright © 2026 Roman Scherer <roman@burningswell.com>
;;;
;;; This file is part of the r0man Guix Home channel.

(define-module (r0man guix tests home gascity)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu services)
  #:use-module (gnu services guix)
  #:use-module (gnu system vm)
  #:use-module (gnu tests)
  #:use-module (guix gexp)
  #:use-module (r0man guix home services gascity)
  #:use-module (r0man guix home services git)
  #:use-module (r0man guix tests)
  #:export (%test-home-gascity
            %test-home-gascity-with-git))

;;; Commentary:
;;;
;;; Marionette test that boots a minimal x86_64 OS containing a
;;; `guix-home-service-type' for user `alice' whose home environment
;;; pairs `home-bash-service-type' (activation canary) with
;;; `home-gascity-service-type'.
;;;
;;; Two instances cover the new multi-instance schema:
;;;
;;;   * `'main'  — file beads, gc-home `.gc-main', no cities (so the
;;;                profile drops dolt/beads-next, exercising the
;;;                package-filtering branch and the
;;;                no-`bd' dolt-seed gate).  Carries non-default
;;;                `dolt-user-name'/`dolt-user-email' so the seed
;;;                JSON proves the identity propagates from the FIRST
;;;                instance (v2 follow-up #3).
;;;   * `'test2' — bd beads, gc-home `.gc-test2', dashboard on 18080,
;;;                supervisor on 9372, one managed city with one
;;;                pack-and-agent declaration.
;;;
;;; Asserts:
;;;
;;;   * .bashrc — generic home-environment activation canary.
;;;   * Provision symbols `gascity-supervisor-main',
;;;     `gascity-supervisor-test2', `gascity-init-main',
;;;     `gascity-init-test2', and `gascity-dashboard-test2' all
;;;     referenced from `~/.config/shepherd/init.scm'.
;;;   * `~/.dolt/config_global.json' seeded (the bd instance triggered
;;;     it) with the FIRST instance's non-default identity — keyed
;;;     substrings (not bare names) so a future regression that emits
;;;     structurally-different JSON cannot pass (v2 follow-up #4).
;;;   * `~/.gc-test2/supervisor.toml' contains `port = 9372'
;;;     (verifies the supervisor-port plumbing actually reaches disk).
;;;   * `~/.gc-test2/cities.toml' lists the test2 city.
;;;   * `~/cities/test2/.guix-home-managed' marker file present.
;;;   * `~/cities/test2/city.toml' contains `[beads]\nprovider = "bd"',
;;;     a `[packs.examples]' entry, and an `[[agent]]' block for the
;;;     declared agent (rendered city.toml).
;;;   * `setup-environment' exports GC_HOME (uses the FIRST instance's
;;;     gc-home, here `.gc-main').
;;;
;;; A second exported test, `%test-home-gascity-with-git', exercises
;;; the negative branch of the dolt-seed short-circuit
;;; (services/gascity.scm activation step (2)): when
;;; `home-git-service-type' has already produced `~/.gitconfig', the
;;; activation MUST NOT write `~/.dolt/config_global.json' — dolt
;;; falls back to git's identity.  Covers v2 follow-up #2.
;;;
;;; Code:

(define %home-gascity-environment
  (home-environment
   (services
    (list (service home-bash-service-type)
          (service home-gascity-service-type
                   (home-gascity-configuration
                    (instances
                     (list (gascity-instance-configuration
                            (name 'main)
                            (gc-home ".gc-main")
                            (beads-provider 'file)
                            ;; Non-default identity proves the FIRST
                            ;; instance's `dolt-user-name' /
                            ;; `dolt-user-email' actually reach
                            ;; ~/.dolt/config_global.json (covers v2
                            ;; follow-up #3 — propagation).  Kept on
                            ;; the file-beads instance so the per-
                            ;; instance default check (the next
                            ;; instance, 'test2, is the bd one that
                            ;; triggers seeding) still uses the FIRST
                            ;; instance's identity — that is the
                            ;; documented seeding behaviour
                            ;; (services/gascity.scm:611-613).
                            (dolt-user-name "Ada Lovelace")
                            (dolt-user-email "ada@example.com"))
                           (gascity-instance-configuration
                            (name 'test2)
                            (gc-home ".gc-test2")
                            (supervisor-port 9372)
                            (dashboard? #t)
                            (dashboard-port 18080)
                            (cities
                             (list (gascity-city-configuration
                                    (path "/home/alice/cities/test2")
                                    (packs
                                     (list (gascity-pack-configuration
                                            (name "examples")
                                            (source "https://example/pack"))))
                                    (agents
                                     (list (gascity-agent-configuration
                                            (name "watcher")
                                            (provider "claude"))))))))))))))))

(define %home-gascity-os
  (r0man-simple-operating-system
   (service guix-home-service-type
            `(("alice" ,%home-gascity-environment)))))

(define (run-home-gascity-test)
  "Run the home-gascity marionette test in a VM."
  (define os
    (marionette-operating-system
     %home-gascity-os
     #:imported-modules '((gnu services herd))))

  (define vm
    (virtual-machine
     (operating-system os)
     (memory-size 2048)))

  (define test
    (with-imported-modules '((gnu build marionette)
                             (asahi guix build marionette)
                             (r0man guix build marionette))
      #~(begin
          (use-modules (srfi srfi-64)
                       (gnu build marionette)
                       (r0man guix build marionette))

          (define marionette
            (make-marionette (cons* #$vm '#$%r0man-marionette-qemu-args)))

          (test-runner-current (system-test-runner #$output))
          (test-begin "home-gascity")

          (test-assert-home-bashrc-exists "alice" marionette)

          ;; Dolt identity is seeded because the 'test2 instance uses
          ;; the default 'bd' beads provider.
          (test-assert-file-exists
           "alice ~/.dolt/config_global.json seeded"
           "/home/alice/.dolt/config_global.json"
           marionette)

          ;; Strict keyed-substring assertions — not bare names — so a
          ;; future regression that emits a structurally-different but
          ;; textually-substring-matching JSON cannot pass (v2
          ;; follow-up #4).  Combined with the non-default identity on
          ;; the FIRST instance above, also verifies the
          ;; dolt-user-{name,email} fields actually propagate from
          ;; configuration to disk (v2 follow-up #3).
          (test-assert-file-contains
           "alice dolt config has \"user.name\":\"Ada Lovelace\""
           "/home/alice/.dolt/config_global.json"
           "\"user.name\":\"Ada Lovelace\""
           marionette)

          (test-assert-file-contains
           "alice dolt config has \"user.email\":\"ada@example.com\""
           "/home/alice/.dolt/config_global.json"
           "\"user.email\":\"ada@example.com\""
           marionette)

          ;; GC_HOME picks up the FIRST instance's gc-home (.gc-main).
          ;; home-environment-variables-service-type emits values via
          ;; shell-double-quote, so the value is wrapped in double
          ;; quotes; the dollar sign is preserved for shell expansion.
          (test-assert-file-contains
           "alice setup-environment exports GC_HOME"
           "/home/alice/.guix-home/setup-environment"
           "GC_HOME=\"$HOME/.gc-main\""
           marionette)

          ;; Suffixed shepherd provision symbols for both instances.
          (test-assert-file-contains
           "alice's shepherd init.scm references gascity-supervisor-main"
           "/home/alice/.guix-home/files/.config/shepherd/init.scm"
           "gascity-supervisor-main"
           marionette)

          (test-assert-file-contains
           "alice's shepherd init.scm references gascity-init-main"
           "/home/alice/.guix-home/files/.config/shepherd/init.scm"
           "gascity-init-main"
           marionette)

          (test-assert-file-contains
           "alice's shepherd init.scm references gascity-supervisor-test2"
           "/home/alice/.guix-home/files/.config/shepherd/init.scm"
           "gascity-supervisor-test2"
           marionette)

          (test-assert-file-contains
           "alice's shepherd init.scm references gascity-init-test2"
           "/home/alice/.guix-home/files/.config/shepherd/init.scm"
           "gascity-init-test2"
           marionette)

          (test-assert-file-contains
           "alice's shepherd init.scm references gascity-dashboard-test2"
           "/home/alice/.guix-home/files/.config/shepherd/init.scm"
           "gascity-dashboard-test2"
           marionette)

          ;; gascity-init's `gc rig add' invocation passes --adopt
          ;; conditionally on <rig>/.beads/metadata.json existing
          ;; (services/gascity.scm:957-964; closes guix-home-7fo).
          ;; The rendered shepherd-gascity-init-*.scm files live under
          ;; /gnu/store with a hashed prefix; init.scm references them
          ;; by absolute path.  Scan the store for any such file that
          ;; serialises both substrings — the literal Scheme is
          ;; preserved verbatim regardless of whether the instance
          ;; actually has rigs at runtime.
          (test-assert
              "gascity-init service body passes --adopt for populated rigs"
            (marionette-eval
             '(begin
                (use-modules (ice-9 ftw)
                             (ice-9 textual-ports))
                (let* ((dir "/gnu/store")
                       (entries (or (scandir
                                     dir
                                     (lambda (f)
                                       (and (string-contains
                                             f "shepherd-gascity-init-")
                                            (string-suffix? ".scm" f))))
                                    '())))
                  (let loop ((es entries))
                    (cond ((null? es) #f)
                          ((let ((c (call-with-input-file
                                        (string-append dir "/" (car es))
                                      get-string-all)))
                             (and (string-contains c "--adopt")
                                  (string-contains c
                                                   "/.beads/metadata.json")))
                           #t)
                          (else (loop (cdr es)))))))
             marionette))

          ;; supervisor.toml carries the user-declared port for test2.
          (test-assert-file-exists
           "alice ~/.gc-test2/supervisor.toml exists"
           "/home/alice/.gc-test2/supervisor.toml"
           marionette)

          (test-assert-file-contains
           "alice ~/.gc-test2/supervisor.toml has port = 9372"
           "/home/alice/.gc-test2/supervisor.toml"
           "port = 9372"
           marionette)

          ;; Managed-city marker + rendered city.toml content.
          (test-assert-file-exists
           "alice ~/cities/test2/.guix-home-managed exists"
           "/home/alice/cities/test2/.guix-home-managed"
           marionette)

          (test-assert-file-contains
           "alice ~/cities/test2/city.toml has [beads] provider = bd"
           "/home/alice/cities/test2/city.toml"
           "provider = \"bd\""
           marionette)

          (test-assert-file-contains
           "alice ~/cities/test2/city.toml has [packs.examples]"
           "/home/alice/cities/test2/city.toml"
           "[packs.examples]"
           marionette)

          (test-assert-file-contains
           "alice ~/cities/test2/city.toml has [[agent]]"
           "/home/alice/cities/test2/city.toml"
           "[[agent]]"
           marionette)

          ;; cities.toml lists the test2 city.
          (test-assert-file-contains
           "alice ~/.gc-test2/cities.toml lists the test2 city"
           "/home/alice/.gc-test2/cities.toml"
           "/home/alice/cities/test2"
           marionette)

          ;; Bash completion: home-bash-extension inlines the
          ;; computed-file content into ~/.bashrc (no `source' line),
          ;; so we look for cobra's bash-completion marker directly.
          (test-assert-file-contains
           "alice ~/.bashrc contains gc bash completion body"
           "/home/alice/.bashrc"
           "__gc_get_completion_results"
           marionette)

          ;; Zsh / fish completions ship under ~/.config/gascity/.
          (test-assert-file-exists
           "alice ~/.config/gascity/completions/gc.zsh exists"
           "/home/alice/.config/gascity/completions/gc.zsh"
           marionette)

          (test-assert-file-exists
           "alice ~/.config/gascity/completions/gc.fish exists"
           "/home/alice/.config/gascity/completions/gc.fish"
           marionette)

          (test-end))))
  (gexp->derivation "r0man-home-gascity-test" test))

(define %test-home-gascity
  (system-test
   (name "r0man-home-gascity")
   (description "Boot a minimal OS with guix-home-service-type wrapping
home-bash-service-type and home-gascity-service-type for alice.  Two
instances ('main' file-beads + 'test2' bd-with-dashboard) verify
multi-instance shepherd provision suffixing, atomic supervisor.toml
write with the user-declared port, sidecar marker file for managed
cities, rendered city.toml [beads]/[packs]/[[agent]] sections, and
custom dolt-user-{name,email} propagation into
~/.dolt/config_global.json with strict keyed-substring assertions.")
   (value (run-home-gascity-test))))


;;;
;;; %test-home-gascity-with-git: ~/.gitconfig short-circuit (negative
;;; branch).
;;;
;;; The dolt-seed activation step in services/gascity.scm:648-662 only
;;; writes ~/.dolt/config_global.json when (a) at least one instance
;;; uses (beads-provider 'bd) AND (b) NEITHER ~/.dolt/config_global.json
;;; NOR ~/.gitconfig already exists.  This second test asserts the
;;; (b) negative branch: home-git-service-type populates ~/.gitconfig,
;;; and the gascity activation MUST short-circuit so dolt's own
;;; git-config fallback (verified in container by gascity-dolt-identity-
;;; seed memory) is what supplies identity at runtime.
;;;

(define %home-gascity-with-git-environment
  (home-environment
   (services
    (list (service home-bash-service-type)
          (service home-git-service-type)
          (service home-gascity-service-type
                   (home-gascity-configuration
                    (instances
                     (list (gascity-instance-configuration
                            (name 'main)
                            (gc-home ".gc-main")
                            ;; (beads-provider 'bd) — default — keeps
                            ;; the (any-bd?) condition true so step
                            ;; (a) of the seed gate passes; the test
                            ;; isolates the (b) ~/.gitconfig branch.
                            )))))))))

(define %home-gascity-with-git-os
  (r0man-simple-operating-system
   (service guix-home-service-type
            `(("alice" ,%home-gascity-with-git-environment)))))

(define (run-home-gascity-with-git-test)
  "Run the home-gascity-with-git marionette test in a VM."
  (define os
    (marionette-operating-system
     %home-gascity-with-git-os
     #:imported-modules '((gnu services herd))))

  (define vm
    (virtual-machine
     (operating-system os)
     (memory-size 2048)))

  (define test
    (with-imported-modules '((gnu build marionette)
                             (asahi guix build marionette)
                             (r0man guix build marionette))
      #~(begin
          (use-modules (srfi srfi-64)
                       (gnu build marionette)
                       (r0man guix build marionette))

          (define marionette
            (make-marionette (cons* #$vm '#$%r0man-marionette-qemu-args)))

          (test-runner-current (system-test-runner #$output))
          (test-begin "home-gascity-with-git")

          ;; Activation completed (canary).
          (test-assert-home-bashrc-exists "alice" marionette)

          ;; home-git-service-type produced ~/.gitconfig — precondition
          ;; for the short-circuit branch we are exercising.
          (test-assert-file-exists
           "alice ~/.gitconfig present (home-git activated)"
           "/home/alice/.gitconfig"
           marionette)

          ;; gascity activation MUST NOT have written
          ;; ~/.dolt/config_global.json: ~/.gitconfig presence
          ;; short-circuits the seed (services/gascity.scm:648-662).
          ;; Dolt falls back to git's user.name/user.email at runtime.
          (test-assert-file-absent
           "alice ~/.dolt/config_global.json absent (gitconfig short-circuit)"
           "/home/alice/.dolt/config_global.json"
           marionette)

          (test-end))))
  (gexp->derivation "r0man-home-gascity-with-git-test" test))

(define %test-home-gascity-with-git
  (system-test
   (name "r0man-home-gascity-with-git")
   (description "Boot a minimal OS with guix-home-service-type wrapping
home-bash-service-type, home-git-service-type, and a single-instance
home-gascity-service-type (default 'bd' beads provider).  Asserts the
gascity dolt-seed step's negative ~/.gitconfig short-circuit branch:
~/.dolt/config_global.json must be ABSENT after activation because
home-git already produced ~/.gitconfig and dolt falls back to git's
identity at runtime.")
   (value (run-home-gascity-with-git-test))))

;;; gascity.scm ends here
