(define-module (r0man guix home clojure)
  #:use-module ((r0man guix packages clojure) #:prefix r0man:)
  #:use-module (gnu home services)
  #:use-module (gnu packages clojure)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (nongnu packages clojure)
  #:export (home-clojure-services))

;; Clojure

(define home-clojure-service
  (simple-service 'clojure-service home-profile-service-type
                  (list r0man:babashka
                        r0man:clojure-lsp
                        ;; r0man:editor-code-assistant
                        leiningen)))

;; Clojure LSP

(define clojure-lsp-files
  `((".config/clojure-lsp/config.edn" ,(local-file "files/clojure-lsp.edn"))))

(define home-clojure-lsp-service
  (simple-service 'clojure-lsp-service home-files-service-type clojure-lsp-files))

(define home-clojure-services
  (list home-clojure-service
        home-clojure-lsp-service))
