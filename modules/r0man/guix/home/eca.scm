(define-module (r0man guix home eca)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:export (home-eca-services))

(define files
  `((".config/eca/AGENTS.md" ,(local-file "files/eca/AGENTS.md"))
    (".config/eca/commands" ,(local-file "files/eca/commands" #:recursive? #t))
    (".config/eca/config.json" ,(local-file "files/eca/config.json"))))

(define home-eca-services
  (list (simple-service 'eca-config home-files-service-type files)))
