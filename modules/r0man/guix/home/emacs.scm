(define-module (r0man guix home emacs)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-build)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages erlang)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (nongnu packages emacs)
  #:use-module (r0man guix packages emacs)
  #:export (home-emacs-configuration
            home-emacs-service-type
            tangle-org-file))

;;; Commentary:
;;;
;;; Home service for GNU Emacs configuration.
;;; Manages Emacs init files with org-babel tangling support and
;;; installs Emacs packages.
;;;
;;; Code:

;; Pre-load org files at module load time when current-filename is available
(define %init-el-org
  (local-file "files/emacs/init.el.org"))

(define %early-init-el-org
  (local-file "files/emacs/early-init.el.org"))

;; Helper to infer output filename from org filename
(define (infer-output-name org-file-name)
  "Infer tangled output name from ORG-FILE-NAME.
   E.g., 'init.el.org' -> 'init.el', 'early-init.el.org' -> 'early-init.el'"
  (let* ((name (basename org-file-name))
         (len (string-length name)))
    (if (and (> len 4) (string=? (substring name (- len 4)) ".org"))
        (substring name 0 (- len 4))
        name)))

;; Helper to tangle an org file during build
(define* (tangle-org-file org-file result-name)
  "Create a computed-file that tangles ORG-FILE to RESULT-NAME.
   ORG-FILE must be a file-like object (e.g., from local-file).
   RESULT-NAME is the output filename (e.g., 'init.el')."
  (let ((org-file org-file)
        (result-name result-name))
    (computed-file
     result-name
     (with-imported-modules '((guix build utils))
       #~(call-with-output-file #$output
           (lambda (out-port)
             (use-modules (guix build utils)
                          (ice-9 popen)
                          (ice-9 rdelim)
                          (srfi srfi-13))

             (let* ((emacs #$(file-append emacs-minimal "/bin/emacs"))
                    (org-file #$org-file)
                    (output-basename #$result-name)
                    (temp-dir (tmpnam))
                    (temp-org (string-append temp-dir "/input.org")))

               ;; Create temporary directory
               (mkdir-p temp-dir)

               ;; Copy org file to temp directory
               (copy-file org-file temp-org)
               (format #t "Tangling ~a...~%" temp-org)

               ;; Run emacs to tangle the file
               (let* ((cmd (list emacs
                                 "--batch"
                                 "--eval" "(require 'org)"
                                 "--eval" "(setq org-confirm-babel-evaluate nil)"
                                 "--eval" (format #f "(cd \"~a\")" temp-dir)
                                 "--eval" (format #f "(find-file \"~a\")" temp-org)
                                 "--eval" (format #f "(org-babel-tangle-file \"~a\" \"~a\")" temp-org output-basename)
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

               ;; Find the tangled output
               (let ((tangled-file (string-append temp-dir "/" output-basename)))
                 (unless (file-exists? tangled-file)
                   (error "Tangling failed - expected output file not created:"
                          tangled-file))

                 ;; Write tangled content to output port
                 (call-with-input-file tangled-file
                   (lambda (in-port)
                     (dump-port in-port out-port)))

                 (format #t "Successfully tangled to output~%")

                 ;; Clean up
                 (delete-file-recursively temp-dir)))))))))

(define default-emacs-packages
  (list (if (target-aarch64?) emacs emacs)
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
        emacs-clipetty
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
        emacs-corfu
        emacs-cov
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
        emacs-inflections
        emacs-inspector
        emacs-jarchive
        emacs-jinx
        emacs-jira
        emacs-jiralib2
        emacs-js2-refactor
        emacs-json-process-client
        emacs-kele
        emacs-kind-icon
        emacs-kotlin-mode
        emacs-kubel
        emacs-language-detection
        emacs-logview
        emacs-lsp-dart
        emacs-lsp-docker
        emacs-lsp-java
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
        emacs-ox-jira
        emacs-ox-slack
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
        emacs-with-simulated-input
        emacs-x509-mode
        emacs-yaml-mode
        emacs-yasnippet
        emacs-yasnippet-snippets
        clhs))

;; Pre-load bookmarks file at module load time
(define %emacs-bookmarks
  (local-file "files/emacs/bookmarks"))

(define-record-type* <home-emacs-configuration>
  home-emacs-configuration make-home-emacs-configuration
  home-emacs-configuration?
  (init-file home-emacs-init-file
             (default (tangle-org-file %init-el-org "init.el"))
             (description "Tangled init.el file."))
  (early-init-file home-emacs-early-init-file
                   (default (tangle-org-file %early-init-el-org "early-init.el"))
                   (description "Tangled early-init.el file."))
  (bookmarks-file home-emacs-bookmarks-file
                  (default %emacs-bookmarks)
                  (description "Emacs bookmarks file."))
  (packages home-emacs-packages
            (default default-emacs-packages)
            (description "List of Emacs-related packages to install.")))

(define (home-emacs-files config)
  "Return alist of Emacs configuration files to deploy."
  `((".emacs.d/init.el" ,(home-emacs-init-file config))
    (".emacs.d/early-init.el" ,(home-emacs-early-init-file config))
    (".emacs.d/bookmarks" ,(home-emacs-bookmarks-file config))))

(define (home-emacs-profile-packages config)
  "Return list of Emacs packages to install."
  (home-emacs-packages config))

(define (home-emacs-bash-extension config)
  "Return bash extension for emacs-vterm integration."
  (home-bash-extension
   (bashrc
    (list (file-append emacs-vterm "/share/emacs/site-lisp/vterm-"
                       (package-version emacs-vterm)
                       "/etc/emacs-vterm-bash.sh")))))

(define home-emacs-service-type
  (service-type
   (name 'home-emacs)
   (extensions
    (list (service-extension home-files-service-type
                             home-emacs-files)
          (service-extension home-profile-service-type
                             home-emacs-profile-packages)
          (service-extension home-bash-service-type
                             home-emacs-bash-extension)))
   (default-value (home-emacs-configuration))
   (description
    "Install and configure GNU Emacs with org-babel tangling support.")))
