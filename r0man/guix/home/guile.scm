(define-module (r0man guix home guile)
  #:use-module (gnu home services)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:export (home-guile-services))

(define files
  `((".guile" ,(local-file "files/guile"))))

(define packages
  (list guildhall
        guile-3.0
        guile-ares-rs
        guile-colorized
        guile-git
        guile-json-4
        guile-lib
        guile-lzlib
        guile-readline
        guile-sqlite3
        guile-zlib))

(define home-guile-services
  (list (simple-service 'guile-config home-files-service-type files)
        (simple-service 'guile-packages home-profile-service-type packages)))
