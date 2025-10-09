(define-module (r0man guix home emacs)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-build)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages erlang)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (nongnu packages emacs)
  #:use-module (r0man guix packages emacs)
  #:export (emacs-shepherd-service home-emacs-services))

;; Helper to tangle any org file during build
(define* (tangle-org-file org-file #:optional (output-name "init.el"))
  "Create a computed-file that tangles ORG-FILE to OUTPUT-NAME"
  (computed-file
   output-name
   (with-imported-modules '((guix build utils))
     #~(begin
         (use-modules (guix build utils)
                      (ice-9 popen)
                      (ice-9 rdelim))

         (let* ((emacs #$(file-append emacs "/bin/emacs"))
                (org-file #$org-file)
                (output-file #$output)
                (temp-dir (tmpnam)))

           ;; Create temporary directory
           (mkdir-p temp-dir)

           ;; Copy org file to temp directory
           (let ((temp-org (string-append temp-dir "/input.org")))
             (copy-file org-file temp-org)

             (format #t "Tangling ~a...~%" temp-org)

             ;; Run emacs to tangle the file
             (let* ((cmd (list emacs
                              "--batch"
                              "--eval" "(require 'org)"
                              "--eval" "(setq org-confirm-babel-evaluate nil)"
                              "--eval" (format #f "(cd \"~a\")" temp-dir)
                              "--eval" (format #f "(find-file \"~a\")" temp-org)
                              "--eval" "(org-babel-tangle)"
                              "--kill"))
                    (port (apply open-pipe* OPEN_READ cmd)))

               ;; Print output
               (let loop ((line (read-line port)))
                 (unless (eof-object? line)
                   (format #t "~a~%" line)
                   (loop (read-line port))))

               ;; Check status
               (let ((status (close-pipe port)))
                 (unless (zero? status)
                   (error "Tangling failed with status" status))))

             ;; Find the tangled output (org-babel-tangle creates it based on :tangle directive)
             (let ((tangled-file (string-append temp-dir "/" #$output-name)))
               (unless (file-exists? tangled-file)
                 (error "Tangling failed - expected output file not created:" tangled-file))

               ;; Copy to final destination
               (copy-file tangled-file output-file)
               (format #t "Successfully tangled to ~a~%" output-file)

               ;; Clean up
               (delete-file-recursively temp-dir))))))))

(define readme-org (local-file "files/emacs/README.org"))
(define init-el (tangle-org-file readme-org "init.el"))

(define files
  `((".emacs.d/early-init.el" ,(local-file "files/emacs/early-init.el"))
    (".emacs.d/init.el" ,init-el)))

(define packages
  (list (if (target-aarch64?) emacs-pgtk emacs)
        emacs-adoc-mode
        emacs-agent-shell
        emacs-aider
        emacs-aidermacs
        emacs-aggressive-indent
        emacs-aio
        emacs-all-the-icons
        emacs-arei
        emacs-auto-dictionary-mode
        emacs-avy
        emacs-avy-menu
        emacs-bluetooth
        emacs-bnf-mode
        emacs-buttercup
        emacs-cape
        emacs-cask
        emacs-chatgpt
        emacs-cider
        emacs-claude-code
        emacs-claude-code-ide
        emacs-claudemacs
        emacs-clj-refactor
        emacs-clojure-mode-extra-font-locking
        emacs-clojure-ts-mode
        emacs-closql
        emacs-codegpt
        emacs-color-theme
        emacs-color-theme-solarized-r0man
        emacs-combobulate
        emacs-consult
        emacs-consult-gh
        emacs-copilot
        emacs-copilot-chat
        emacs-corfu
        emacs-corfu
        emacs-dall-e
        emacs-dart-mode
        emacs-debbugs
        emacs-docker
        emacs-docker-compose-mode
        emacs-dockerfile-mode
        emacs-docopt
        emacs-doric-themes
        emacs-dracula-theme
        emacs-eat
        emacs-edit-indirect
        emacs-efrit
        ;; emacs-editor-code-assistant
        emacs-ef-themes
        ;; emacs-eglot
        ;; emacs-eglot-java
        emacs-el-mock
        emacs-eldev
        emacs-eldoc-box
        emacs-elfeed
        emacs-elisa
        emacs-elisp-slime-nav
        emacs-elixir-mode
        emacs-ellama
        emacs-emacsql
        emacs-emacsql-sqlite
        emacs-embark
        emacs-emms
        ;; emacs-enwc
        emacs-engine-mode
        emacs-erlang
        emacs-eval-expr
        emacs-evil
        emacs-exec-path-from-shell
        emacs-expand-region
        emacs-find-file-in-project
        emacs-flx
        emacs-flycheck
        emacs-flycheck-clj-kondo
        emacs-flycheck-elsa
        emacs-flycheck-flow
        emacs-flymd
        emacs-forge
        emacs-fsm
        emacs-geiser
        emacs-geiser-guile
        emacs-gif-screencast
        emacs-git-email
        emacs-git-timemachine
        emacs-github-browse-file
        emacs-gnuplot
        emacs-go-mode
        emacs-google-translate
        emacs-gptel
        emacs-graphql
        emacs-graphql-mode
        emacs-guess-language
        emacs-guix
        emacs-haskell-mode
        emacs-helpful
        emacs-htmlize
        emacs-hy-mode
        emacs-ido-vertical-mode
        emacs-inflections
        emacs-inspector
        emacs-jarchive
        emacs-jinx
        emacs-jira
        emacs-jiralib2
        emacs-js2-refactor
        emacs-json-process-client
        emacs-kotlin-mode
        emacs-kubel
        emacs-language-detection
        emacs-logview
        emacs-lsp-dart
        emacs-lsp-docker
        emacs-lsp-java
        emacs-lsp-metals
        emacs-lsp-treemacs
        emacs-lsp-ui
        emacs-macrostep
        emacs-magit
        emacs-marginalia
        emacs-markdown-mode
        emacs-markdown-preview-eww
        emacs-markdown-preview-mode
        emacs-mcp
        emacs-mcp-client
        emacs-mcp-server-lib
        emacs-mermaid-mode
        emacs-mu4e-alert
        emacs-multi-term
        emacs-multi-vterm
        emacs-multiple-cursors
        emacs-nano-theme
        emacs-nord-theme
        emacs-oauth2
        emacs-ob-mermaid
        emacs-openwith
        emacs-orderless
        emacs-org
        emacs-org-contrib
        emacs-org-gcal
        emacs-org-jira
        emacs-org-make-toc
        emacs-org-present
        emacs-org-reveal
        emacs-org-roam
        emacs-org-tree-slide
        emacs-osm
        emacs-ox-gfm
        emacs-ox-gfm
        emacs-ox-jira
        emacs-ox-slack
        emacs-paimon
        emacs-pandoc-mode
        emacs-paredit
        emacs-parsec
        emacs-pass
        emacs-pgemacs
        emacs-plz
        emacs-popwin
        emacs-posframe
        emacs-projectile
        emacs-pulsar
        emacs-rainbow-mode
        emacs-redshank
        emacs-refactor
        emacs-request-deferred
        emacs-sbt-mode
        emacs-scala-mode
        emacs-scss-mode
        emacs-selectrum
        emacs-semext
        emacs-show-font
        emacs-slite
        emacs-sly
        emacs-sly-asdf
        emacs-sly-macrostep
        emacs-sly-stepper
        emacs-sql-indent
        emacs-standard-themes
        emacs-stumpwm-mode
        emacs-svg-lib
        emacs-terraform-mode
        emacs-timesheet
        emacs-transient
        emacs-treemacs
        emacs-undercover
        emacs-undo-tree
        emacs-unfill
        emacs-use-package
        emacs-vertico
        emacs-virtualenvwrapper
        emacs-vterm
        emacs-web-mode
        emacs-which-key
        emacs-whisper
        emacs-x509-mode
        emacs-yaml-mode
        emacs-yasnippet
        emacs-yasnippet-snippets
        clhs))

(define home-emacs-services
  (list (simple-service 'emacs-files home-files-service-type files)
        (simple-service 'emacs-packages home-profile-service-type packages)))
