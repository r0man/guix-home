(define-module (r0man home emacs)
  #:use-module (flat packages emacs)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (r0man packages emacs)
  #:export (emacs-shepherd-service home-emacs-services))

(define packages
  (list emacs-aggressive-indent
        emacs-aio
        emacs-auto-dictionary-mode
        emacs-avy
        emacs-avy-menu
        emacs-bluetooth
        emacs-cask
        emacs-cider
        emacs-clj-refactor
        emacs-clojure-mode-extra-font-locking
        emacs-color-theme
        emacs-company-box
        emacs-company-lsp
        emacs-company-quickhelp
        emacs-consult
        emacs-dap-mode
        emacs-dart-mode
        emacs-docker
        emacs-docker-compose-mode
        emacs-dockerfile-mode
        emacs-docopt
        emacs-edit-indirect
        emacs-el-mock
        emacs-elfeed
        emacs-elisp-slime-nav
        emacs-elpy
        emacs-embark
        emacs-emms
        emacs-engine-mode
        emacs-eval-expr
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
        emacs-github-browse-file
        emacs-gnuplot
        emacs-go-mode
        emacs-graphql
        emacs-graphql-mode
        emacs-guess-language
        emacs-guix
        emacs-haskell-mode
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
        emacs-lsp-dart
        emacs-lsp-docker
        emacs-lsp-java
        emacs-lsp-metals
        emacs-lsp-mode
        emacs-lsp-treemacs
        emacs-lsp-ui
        emacs-macrostep
        emacs-magit
        emacs-marginalia
        emacs-markdown-mode
        emacs-markdown-preview-eww
        emacs-markdown-preview-mode
        emacs-multi-term
        emacs-multiple-cursors
        emacs-native-comp
        emacs-oauth2
        emacs-orderless
        emacs-org
        emacs-org-contrib
        emacs-org-gcal
        emacs-org-jira
        emacs-org-present
        emacs-org-reveal
        emacs-org-tree-slide
        emacs-ox-jira
        emacs-ox-pandoc
        emacs-paimon
        emacs-pandoc-mode
        emacs-paredit
        emacs-parsec
        emacs-pass
        emacs-plantuml-mode
        emacs-popwin
        emacs-posframe
        emacs-projectile
        emacs-rainbow-mode
        emacs-redshank
        emacs-refactor
        emacs-request-deferred
        emacs-sbt-mode
        emacs-scala-mode
        emacs-scss-mode
        emacs-selectrum
        emacs-slack
        emacs-slime
        emacs-slime-company
        emacs-smex
        emacs-smooth-scrolling
        emacs-stumpwm-mode
        emacs-terraform-mode
        emacs-timesheet
        emacs-treemacs
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

(define emacs-shepherd-service
  (shepherd-service
   (documentation "Emacs")
   (provision '(emacs))
   (start #~(make-forkexec-constructor
             (list #$(file-append emacs-native-comp "/bin/emacs")
                   "--fg-daemon")
             #:log-file (string-append
			 (or (getenv "XDG_LOG_HOME")
			     (format #f "~a/.local/var/log"
				     (getenv "HOME")))
			 "/emacs.log")))
   (stop #~(make-kill-destructor))))

(define home-emacs-services
  (list (simple-service 'emacs-packages home-profile-service-type packages)))