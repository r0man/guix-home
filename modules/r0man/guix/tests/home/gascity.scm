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
  #:use-module (r0man guix tests)
  #:export (%test-home-gascity))

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
;;;                no-`bd' dolt-seed gate).
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
;;;     it).
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
                            (beads-provider 'file))
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

          (test-assert-file-contains
           "alice dolt config has default user.name"
           "/home/alice/.dolt/config_global.json"
           "Gas City"
           marionette)

          ;; GC_HOME picks up the FIRST instance's gc-home (.gc-main).
          (test-assert-file-contains
           "alice setup-environment exports GC_HOME"
           "/home/alice/.guix-home/setup-environment"
           "GC_HOME=$HOME/.gc-main"
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
cities, and rendered city.toml [beads]/[packs]/[[agent]] sections.")
   (value (run-home-gascity-test))))

;;; gascity.scm ends here
