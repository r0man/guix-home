(define-module (r0man home packages)
  #:use-module (flat packages emacs)
  #:use-module (gnu packages base)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages geo)
  #:use-module (gnu packages gimp)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages inkscape)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages man)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages uml)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages)
  #:use-module (r0man packages emacs)
  #:export (packages))

(define packages
  (list apache-arrow
        cmake
        emacs-aggressive-indent
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
        emacs-yasnippet-snippets
        font-dejavu
        font-gnu-freefont
        font-google-roboto
        font-inconsolata
        font-terminus
        fontconfig
        gcc-toolchain
        gdal
        gfortran-toolchain
        gimp
        git
        git-crypt
        glibc-locales
        gnu-make
        gnuplot
        gnutls
        guile-3.0
        guile-git
        guile-lzlib
        guile-sqlite3
        guile-zlib
        help2man
        inkscape
        isync
        lapack
        libatasmart
        libffi
        libgcrypt
        libvterm
        mu
        nss-certs
        openblas
        openblas-ilp64
        pkg-config
        plantuml
        python
        python-cython
        python-lsp-server
        python-numpy
        python-pip
        rofi
        shared-mime-info
        sqitch
        sqlite
        strace
        stumpish
        stumpwm))

;; Emacs packages not yet available

;; ein
;; emacs-pretty-lambdada
;; ensime
;; highline
;; indium
;; kubernetes
;; org-ac
;; org-plus-contrib
;; slamhound
;; solarized-theme
;; soundklaus
;; tree-sitter
;; tree-sitter-langs
