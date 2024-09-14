(use-modules ;; (r0man guix system m1)
             (gnu system)
             (gnu packages)
             (guix gexp)
             (guix git-download)
             (guix packages)
             (guix profiles)
             (guix transformations))

(define %version "0.1.0")

;; (define %m1-system
;;   (manifest-entry
;;     (name "m1-system")
;;     (version %version)
;;     (item m1-operating-system)))

;; (define %systems
;;   (manifest (list %m1-system)))

;; (concatenate-manifests (list %systems))

(specifications->manifest '("hello"))
