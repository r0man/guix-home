(define-module (r0man guix home clojure)
  #:use-module ((r0man guix packages clojure) #:prefix r0man:)
  #:use-module (gnu home services)
  #:use-module (gnu packages clojure)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (nongnu packages clojure)
  #:export (home-clojure-configuration
            home-clojure-service-type))

;;; Commentary:
;;;
;;; Home service for Clojure development environment configuration.
;;; Manages clojure-lsp config and installs Clojure tooling.
;;;
;;; Code:

(define-record-type* <home-clojure-configuration>
  home-clojure-configuration make-home-clojure-configuration
  home-clojure-configuration?
  (lsp-config-file home-clojure-lsp-config-file
                   (default (local-file "files/clojure-lsp.edn"))
                   (description "Path to clojure-lsp config.edn file."))
  (packages home-clojure-packages
            (default (list r0man:babashka
                           r0man:bbin
                           r0man:clojure-lsp
                           leiningen))
            (description "List of Clojure-related packages to install.")))

(define (home-clojure-files config)
  "Return alist of Clojure configuration files to deploy."
  `((".config/clojure-lsp/config.edn"
     ,(home-clojure-lsp-config-file config))))

(define (home-clojure-profile-packages config)
  "Return list of Clojure packages to install."
  (home-clojure-packages config))

(define home-clojure-service-type
  (service-type
   (name 'home-clojure)
   (extensions
    (list (service-extension home-files-service-type
                             home-clojure-files)
          (service-extension home-profile-service-type
                             home-clojure-profile-packages)))
   (default-value (home-clojure-configuration))
   (description
    "Install and configure Clojure development environment for the user.")))
