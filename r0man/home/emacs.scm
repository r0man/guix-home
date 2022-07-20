(define-module (r0man home emacs)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (r0man packages emacs)
  #:export (emacs-shepherd-service home-emacs-services))

(define packages
  (list emacs-aggressive-indent
        ;; emacs-lsp-mode
        emacs-aio
        emacs
        emacs-auto-dictionary-mode
        emacs-avy
        emacs-avy-menu
        emacs-bluetooth
        emacs-buttercup
        emacs-cask
        emacs-cider
        emacs-clj-refactor
        emacs-clojure-mode-extra-font-locking
        emacs-closql
        emacs-color-theme
        emacs-company-box
        emacs-company-lsp
        emacs-company-quickhelp
        emacs-consult
        emacs-corfu
        emacs-dap-mode
        emacs-dart-mode
        emacs-debbugs
        emacs-docker
        emacs-docker-compose-mode
        emacs-dockerfile-mode
        emacs-docopt
        emacs-edit-indirect
        emacs-eglot
        emacs-el-mock
        emacs-elfeed
        emacs-elisp-slime-nav
        emacs-elixir-mode
        emacs-elpy
        emacs-emacsql
        emacs-emacsql-libsqlite3
        emacs-emacsql-sqlite
        emacs-embark
        emacs-emms
        emacs-engine-mode
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
        emacs-geiser
        emacs-geiser-guile
        emacs-gif-screencast
        emacs-git-email
        emacs-git-timemachine
        emacs-github-browse-file
        emacs-gnuplot
        emacs-go-mode
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
        emacs-multi-term
        emacs-multi-vterm
        emacs-multiple-cursors
        emacs-oauth2
        emacs-orderless
        emacs-org
        emacs-org-contrib
        emacs-org-gcal
        emacs-org-jira
        emacs-org-present
        emacs-org-reveal
        emacs-org-roam
        emacs-org-tree-slide
        emacs-ox-gfm
        emacs-ox-jira
        emacs-ox-pandoc
        emacs-paimon
        emacs-pandoc-mode
        emacs-paredit
        emacs-parsec
        emacs-pass
        emacs-plantuml-mode
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
        emacs-slime
        emacs-slime-company
        emacs-smex
        emacs-smooth-scrolling
        emacs-sqlite3-api
        emacs-stumpwm-mode
        emacs-terraform-mode
        emacs-timesheet
        emacs-treemacs
        emacs-undercover
        emacs-undo-tree
        emacs-use-package
        emacs-vertico
        emacs-virtualenvwrapper
        emacs-vterm
        emacs-web-mode
        emacs-which-key
        emacs-x509-mode
        emacs-yaml-mode
        emacs-yasnippet
        emacs-yasnippet-snippets))

(define home-emacs-services
  (list (simple-service 'emacs-packages home-profile-service-type packages)))
