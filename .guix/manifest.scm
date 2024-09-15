(use-modules (r0man guix home systems m1)
             (r0man guix system m1)
             (gnu home)
             (gnu system)
             (gnu packages)
             (guix gexp)
             (guix git-download)
             (guix packages)
             (guix profiles)
             (guix transformations))

(define %version "0.1.0")

(define %m1-home-environment
  (manifest-entry
    (name "m1-home-environment")
    (version %version)
    (item m1-home-environment)))

(define %m1-operating-system
  (manifest-entry
    (name "m1-operating-system")
    (version %version)
    (item m1-operating-system)))

(define %home-environments
  (manifest (list %m1-home-environment)))

(define %operating-systems
  (manifest (list %m1-operating-system)))

(concatenate-manifests (list %home-environments %operating-systems))
