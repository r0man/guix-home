;;; init.el --- My Emacs configuration. -*- lexical-binding: t; -*-

(setq package-archives
      '(("melpa" . "http://melpa.org/packages/")
        ("melpa-stable" . "http://stable.melpa.org/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/")))

(defun current-theme ()
  "Return the theme, depending on the current month and hour of the day."
  (let ((month (string-to-number (format-time-string "%m")))
        (hour (string-to-number (format-time-string "%H"))))
    (cond
     ;; Use dark theme in winter
     ((member month '(1 2 3 10 11 12))
      'solarized-dark)
     ((member hour '(10 11 12 13 14 15 16))
      'solarized-light)
     (t 'solarized-dark))))

(let ((theme 'nord))
  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (select-frame frame)
                  (load-theme theme)))
    (load-theme theme))
  (add-hook 'after-init-hook
            (lambda ()
              (load-theme theme))))

(defun set-font-laptop ()
  "Set the default font for use without external monitor."
  (interactive)
  (set-frame-font "Inconsolata-14" nil t))

(defun set-font-monitor ()
  "Set the default font for use with external monitor."
  (interactive)
  (set-frame-font "Inconsolata-12" nil t))

(defun current-font-size ()
  "Get the font size for the current system."
  (cond ((and (equal "m1" (system-name)))
         (if (getenv "WAYLAND_DISPLAY")
             14
           14))
        ((and (equal "precision" (system-name)))
         14)
        (t 14)))

(add-to-list 'default-frame-alist `(font . ,(format "Inconsolata-%s" (current-font-size))))

(setq comp-num-cpus 4)
(setq comp-deferred-compilation t)

(setq native-comp-async-report-warnings-errors nil)

(setq read-process-output-max (* 1024 1024))

(prefer-coding-system 'utf-8)

(set-language-environment "UTF-8")

(set-default-coding-systems 'utf-8)

(setq frame-resize-pixelwise t)

(set-frame-parameter (selected-frame) 'alpha '(85 50))
(add-to-list 'default-frame-alist '(alpha 85 50))

(require 'package)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-verbose t)

(setq use-package-minimum-reported-time 0)

(unless (file-exists-p "/gnu/store")
  (require 'use-package-ensure)
  (setq use-package-always-ensure t))

(defvar my-external-monitor "DP-1"
  "The name of the external monitor.")

(defun monitor-connected-p (monitor)
  "Return t if MONITOR is connected, otherwise nil."
  (not (string= "" (shell-command-to-string (format "xrandr --listmonitors | grep %s" monitor)))))

(use-package abbrev
  :diminish
  :hook ((text-mode prog-mode) . abbrev-mode)
  :custom
  ;; Set the name of file from which to read abbrevs.
  (abbrev-file-name "~/.config/emacs/abbrev_defs")
  ;; Silently save word abbrevs too when files are saved.
  (save-abbrevs 'silently))

(use-package agent-shell
  :commands (agent-shell-anthropic-start-claude-code)
  :custom
  (agent-shell-anthropic-authentication
   (agent-shell-anthropic-make-authentication
    :api-key (lambda () (auth-source-pick-first-password :host "api.anthropic.com" :user "personal")))))

(use-package aide
  :commands (aide-openai-completion-region-insert)
  :load-path ("~/workspace/aide.el"))

(use-package aider
  :commands (aider-run-aider)
  :custom
  (aider-args '("--openai-api-key=dummy" "--openai-api-base=http://127.0.0.1:8899/v1")))

(use-package aidermacs
  :bind (("C-c a" . aidermacs-transient-menu))
  :config
  (setenv "OPENAI_API_BASE" "http://127.0.0.1:8899/v1")
  (setenv "OPENAI_API_KEY" "dummy")
  :custom
  (aidermacs-backend 'vterm)
  (aidermacs-use-architect-mode t)
  (aidermacs-default-model "gpt-4o"))

(use-package aio
  :defer t)

(defun colorize-current-buffer ()
  (interactive)
  (require 'ansi-color)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))

(use-package autorevert
  :defer 2
  :config
  (global-auto-revert-mode t))

(use-package bug-reference
  :hook ((erc-mode . bug-reference-mode)
         (gnus-article-mode . bug-reference-mode)
         (gnus-mode . bug-reference-mode)
         (gnus-summary-mode . bug-reference-mode)
         (prog-mode . bug-reference-prog-mode))
  :custom
  (bug-reference-bug-regexp
   (rx (group (or (seq word-boundary
                       (or (seq (char "Bb") "ug"
                                (zero-or-one " ")
                                (zero-or-one "#"))
                           (seq (char "Pp") "atch"
                                (zero-or-one " ")
                                "#")
                           (seq (char "Ff") "ixes"
                                (zero-or-one ":")
                                (zero-or-one " ") "#")
                           (seq "RFE"
                                (zero-or-one " ") "#")
                           (seq "PR "
                                (one-or-more (char "a-z+-")) "/"))
                       (group (one-or-more (char "0-9"))
                              (zero-or-one
                               (seq "#" (one-or-more
                                         (char "0-9"))))))
                  (seq (? "<") "https://bugs.gnu.org/"
                       (group-n 2 (one-or-more (char "0-9")))
                       (? ">"))
                  (seq (? "<") "https://issues.guix.gnu.org/"
                       (? "issue/")
                       (group-n 2 (one-or-more (char "0-9")))
                       (? ">"))))))
  (bug-reference-url-format "https://issues.guix.gnu.org/%s"))

(setq calendar-time-display-form
      '(24-hours ":" minutes
                 (if time-zone " (") time-zone (if time-zone ")")))

(use-package chatgpt
  :commands (chatgpt chatgpt-new))

(defun load-if-exists (file)
  "Load `file` if it exists."
  (when (file-exists-p file)
    (load file)))

(defun indent-buffer ()
  "Indent the whole buffer."
  (interactive)
  (if (and (fboundp 'eglot-managed-p)
           (eglot-managed-p))
      (eglot-format-buffer)
    (indent-region (point-min) (point-max))))

(defun untabify-buffer ()
  "Remove all tabs from the current buffer."
  (interactive)
  (untabify (point-min) (point-max)))

(defun cleanup-buffer ()
  "Cleanup the current buffer."
  (interactive)
  (indent-buffer)
  (delete-trailing-whitespace))

(defun sudo-edit (&optional arg)
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun swap-buffers ()
  "Swap your buffers."
  (interactive)
  (cond ((not (> (count-windows)1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
         (while  (< i numWindows)
           (let* ((w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))
                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))
                  (s1 (window-start w1))
                  (s2 (window-start w2)))
             (set-window-buffer w1  b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))

(defun rotate-buffers ()
  "Rotate your buffers."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defun what-face (pos)
  "Show the face found at the current point."
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun xresources ()
  "Reload the ~/.Xresources configuration."
  (interactive)
  (shell-command "xrdb -merge ~/.Xresources ")
  (message "X resources reloaded."))

(defun insert-clj-uuid (n)
  "Insert a Clojure UUID tagged literal in the form of #uuid
  \"11111111-1111-1111-1111-111111111111\". The prefix argument N
  specifies the padding used."
  (interactive "P")
  (let ((n (or n 1)))
    (if (or (< n 0) (> n 9))
        (error "Argument N must be between 0 and 9."))
    (let ((n (string-to-char (number-to-string n))))
      (insert
       (format "#uuid \"%s-%s-%s-%s-%s\""
               (make-string 8 n)
               (make-string 4 n)
               (make-string 4 n)
               (make-string 4 n)
               (make-string 12 n))))))

(defun zprint-buffer ()
  "Run the current buffer through zprint."
  (interactive)
  (shell-command-on-region (point-min) (point-max) "zprint" nil t)
  (goto-char (point-min))
  (deactivate-mark))

(use-package claude-code-ide
  :commands (claude-code-ide)
  :bind ("C-c C-'" . claude-code-ide-menu)
  :custom
  (claude-code-ide-use-side-window nil)
  :config
  (claude-code-ide-emacs-tools-setup))

(use-package copilot
  :when (member (system-name) '("precision"))
  :hook ((clojure-mode . copilot-mode)
         (clojure-ts-mode . copilot-mode)
         (clojurec-mode . copilot-mode)
         (clojurescript-mode . copilot-mode)
         (emacs-lisp-mode . copilot-mode)
         (prog-mode . copilot-mode))
  :bind (:map copilot-completion-map
              ("C-<return>" . 'copilot-accept-completion)
              ("M-n" . 'copilot-next-completion)
              ("M-p" . 'copilot-previous-completion)))

(use-package copilot-chat
  :when (member (system-name) '("precision")))

(use-package corfu
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("C-<tab>" . corfu-previous))
  :custom
  (corfu-cycle t)
  (corfu-preselect 'first)
  :hook ((after-init . global-corfu-mode)))

(use-package cape
  :after corfu
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-elisp-symbol))

(use-package datomic
  :commands (datomic)
  :load-path
  ("~/workspace/datomic.el/src"
   "~/workspace/datomic.el/test"))

(use-package debbugs
  :commands (debbugs-gnu)
  :custom
  ;; Open the issue directly within Emacs.
  (debbugs-browse-url-regexp
   (rx line-start
       "http" (zero-or-one "s") "://"
       (or "debbugs" "issues.guix" "bugs")
       ".gnu.org" (one-or-more "/")
       (group (zero-or-one "cgi/bugreport.cgi?bug="))
       (group-n 3 (one-or-more digit))
       line-end))
  ;; Change the default when run as 'M-x debbugs-gnu'.
  (debbugs-gnu-default-packages '("guix" "guix-patches"))
  ;; Show feature requests.
  (debbugs-gnu-default-severities
   '("serious" "important" "normal" "minor" "wishlist")))

(use-package debbugs-browse
  :hook
  ((bug-reference-mode . debbugs-browse-mode)
   (bug-reference-prog-mode . debbugs-browse-mode)))

(use-package parsec
  :defer t)

(use-package docopt
  :commands docopt
  :load-path
  ("~/workspace/docopt.el/src"
   "~/workspace/docopt.el/test"))

(use-package eca
  :load-path ("~/workspace/eca-emacs")
  :commands (eca)
  :custom
  (eca-chat-use-side-window nil)
  (eca-custom-command '("~/workspace/eca/eca" "server" "--log-level" "debug" "--verbose")))

(use-package efrit
  :commands (efrit-chat
             efrit-do
             efrit-do-async
             efrit-remote-queue
             efrit-streamlined-send
             efrit-unified-do))

(use-package eldoc
  :diminish
  :hook ((c-mode-common . eldoc-mode)
         (emacs-lisp-mode . eldoc-mode)
         (scheme-mode . eldoc-mode))
  :custom
  (eldoc-echo-area-display-truncation-message nil)
  (eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-echo-area-use-multiline-p 3))

(use-package exec-path-from-shell
  :init
  (setq exec-path-from-shell-variables
        '("CHROME_EXECUTABLE"
          "EDITOR"
          "GOOGLE_APPLICATION_CREDENTIALS"
          "MANPATH"
          "METALS_JAVA_OPTS"
          "METALS_JDK_PATH"
          "NPM_PACKAGES"
          "NUCLI_HOME"
          "NUCLI_PY_FULL"
          "NU_COUNTRY"
          "NU_HOME"
          "PATH"
          "SPARK_HOME"
          "XDG_CONFIG_DIRS"
          "XDG_DATA_DIRS"))
  (exec-path-from-shell-initialize))

(setq mac-option-key-is-meta t)
(setq mac-right-option-modifier nil)

(use-package adoc-mode
  :mode (("\\.adoc\\'" . adoc-mode)))

(use-package arei
  :commands (arei))

(use-package avy
  :bind (("M-j" . avy-goto-char-timer)
         :map isearch-mode-map
         ("C-'" . avy-search)))

(use-package bluetooth
  :commands bluetooth-list-devices)

(use-package bnf-mode
  :mode (("\\.bnf\\'" . bnf-mode)))

(use-package clhs
  :defer t
  :init (clhs-setup))

(defun hyperspec-lookup--hyperspec-lookup-eww (orig-fun &rest args)
  (let ((browse-url-browser-function 'eww-browse-url))
    (apply orig-fun args)))

(advice-add 'hyperspec-lookup :around #'hyperspec-lookup--hyperspec-lookup-eww)

(use-package eglot
  :config
  (add-to-list 'eglot-server-programs '(elixir-mode . ("~/workspace/elixir-ls/release/language_server.sh")))
  (add-to-list 'eglot-server-programs '(java-mode . ("~/.config/emacs/share/eclipse.jdt.ls/bin/jdtls")))
  (add-to-list 'eglot-server-programs '(scala-mode . ("metals")))
  ;; (add-to-list 'eglot-server-programs `(scheme-mode . ("guile-lsp-server")))
  ;; (add-hook 'eglot-managed-mode-hook
  ;;           ;; This displays full docs for clojure functions.
  ;;           ;; See https://github.com/joaotavora/eglot/discussions/894
  ;;           #'(lambda ()
  ;;               (setq-local eldoc-documentation-strategy
  ;;                           #'eldoc-documentation-compose

  ;;                           eldoc-echo-area-use-multiline-p
  ;;                           5)))
  :custom
  (eglot-connect-timeout 120)
  (eglot-extend-to-xref t))

(use-package elfeed
  :commands (elfeed)
  :config
  (setq elfeed-feeds
        '("http://planet.clojure.in/atom.xml"
          "https://grumpyhacker.com/feed.xml"
          "https://nullprogram.com/feed"
          "https://planet.emacslife.com/atom.xml"
          "https://sulami.github.io/atom.xml"
          "http://planet.lisp.org/rss20.xml"
          "https://planet.scheme.org/atom.xml")))

(use-package llm
  :defer t
  :custom
  (llm-log t))

(use-package ellama
  :commands (ellama-ask-about
             ellama-ask-line
             ellama-ask-selection
             ellama-code-complete
             ellama-code-edit
             ellama-code-improve
             ellama-code-review
             ellama-complete
             ellama-context-add-buffer
             ellama-context-add-file
             ellama-context-add-info-node
             ellama-context-add-selection
             ellama-define-word
             ellama-improve-conciseness
             ellama-improve-grammar
             ellama-improve-wording
             ellama-load-session
             ellama-provider-select
             ellama-session-switch
             ellama-summarize
             ellama-summarize-killring
             ellama-summarize-webpage
             ellama-translate
             ellama-translate-buffer)
  :custom
  (ellama-auto-scroll t)
  (ellama-language "German")
  :config
  (require 'llm-openai)
  (require 'llm-vertex)
  (add-to-list 'ellama-providers
               (cons "Nu Gemini 1.0"
                     (make-llm-vertex
                      :chat-model "gemini-pro"
                      :project "iteng-itsystems")))
  (add-to-list 'ellama-providers
               (cons "Nu Gemini 1.5"
                     (make-llm-vertex
                      :chat-model "gemini-1.5-pro-preview-0215"
                      :project "iteng-itsystems")))
  (add-to-list 'ellama-providers
               (cons "Nu OpenAI Local Proxy"
                     (make-llm-openai-compatible
                      :url "http://127.0.0.1:8899/v1/"
                      :chat-model "gpt-4.1")))
  (add-to-list 'ellama-providers
               (cons "OpenAI GPT-4.1"
                     (make-llm-openai
                      :key (auth-source-pick-first-password :host "openai.com" :user "ellama")
                      :chat-model "gpt-4.1")))
  (add-to-list 'ellama-providers
               (cons "OpenAI o4-mini"
                     (make-llm-openai
                      :key (auth-source-pick-first-password :host "openai.com" :user "ellama")
                      :chat-model "o4-mini")))
  (add-to-list 'ellama-providers
               (cons "OpenAI o3"
                     (make-llm-openai
                      :key (auth-source-pick-first-password :host "openai.com" :user "ellama")
                      :chat-model "o3")))
  (add-to-list 'ellama-providers
               (cons "Nu LiteLLM OpenAI GPT-5"
                     (make-llm-openai-compatible
                      :url "https://ist-prod-litellm.nullmplatform.com/"
                      :key (auth-source-pick-first-password :host "ist-prod-litellm.nullmplatform.com" :user "apikey")
                      :chat-model "gpt-5"
                      :embedding-model "text-embedding-3-small")))
  (setq ellama-provider (alist-get "Nu LiteLLM OpenAI GPT-5" ellama-providers nil nil #'string=)))

(defun ellama-chat-whisper ()
  "Record audio in a temporary buffer with the `whisper-run`
command. When the user presses a key, stop the recording with by
invoking `whisper-run` again.  The text in the temporary buffer
is then passwd to the ellama-chat command."
  (interactive)
  (require 'ellama)
  (require 'whisper)
  (let ((buffer (get-buffer-create whisper--stdout-buffer-name)))
    (with-current-buffer buffer
      (erase-buffer)
      (make-local-variable 'whisper-after-transcription-hook)
      (add-hook 'whisper-after-transcription-hook
                (lambda ()
                  (let ((transcription (buffer-substring (line-beginning-position)
                                                         (line-end-position))))

                    (ellama-chat transcription)))
                nil t)
      (let ((recording-process (whisper-run)))
        (message "Recording, then asking Ellama. Press RET to stop.")
        (while (not (equal ?\r (read-char)))
          (sit-for 0.5))
        (whisper-run)))))


(defun ellama-ask-about-whisper ()
  "Record audio in a temporary buffer with the `whisper-run`
command. When the user presses a key, stop the recording with by
invoking `whisper-run` again.  The text in the temporary buffer
is then passwd to the ellama-chat command."
  (interactive)
  (require 'ellama)
  (require 'whisper)
  (let ((about-buffer (current-buffer))
        (buffer (get-buffer-create whisper--stdout-buffer-name)))
    (with-current-buffer buffer
      (erase-buffer)
      (make-local-variable 'whisper-after-transcription-hook)
      (add-hook 'whisper-after-transcription-hook
                (lambda ()
                  (let ((transcription (buffer-substring (line-beginning-position)
                                                         (line-end-position))))
                    (with-current-buffer about-buffer
                      (if (region-active-p)
                          (ellama-context-add-selection)
                        (ellama-context-add-buffer (buffer-name (current-buffer)))))
                    (ellama-chat transcription)))
                nil t)
      (let ((recording-process (whisper-run)))
        (message "Recording, then asking Ellama. Press RET to stop.")
        (while (not (equal ?\r (read-char)))
          (sit-for 0.5))
        (whisper-run)))))

(use-package elixir-mode
  :bind (:map elixir-mode-map
              ("C-c C-f" . elixir-format)))

(use-package eval-expr
  :hook ((emacs-lisp-mode . eval-expr-install)))

(use-package clojure-mode
  :after (nu)
  :mode (("\\.edn\\'" . clojure-mode)
         ("\\.cljs\\'" . clojurescript-mode)
         ("\\.cljx\\'" . clojurex-mode)
         ("\\.cljc\\'" . clojurec-mode))
  :config
  (add-hook 'clojure-mode-hook #'subword-mode)
  (define-key clojure-mode-map (kbd "C-c t") #'projectile-toggle-between-implementation-and-test)
  (define-clojure-indent
   (assoc 1)
   (match? 0)
   (time! 1)
   (fdef 1)
   (providing 1)
   ;; cljs.test
   (async 1)
   ;; ClojureScript
   (this-as 1)
   ;; COMPOJURE
   (ANY 2)
   (DELETE 2)
   (GET 2)
   (HEAD 2)
   (POST 2)
   (PUT 2)
   (context 2)
   ;; ALGO.MONADS
   (domonad 1)
   ;; Om.next
   (defui '(1 nil nil (1)))
   ;; CUSTOM
   (api-test 1)
   (web-test 1)
   (database-test 1)
   (defroutes 'defun)
   (flow 'defun)
   (for-all '(1 (2)))
   (assoc-some 1)
   (let-entities 2)
   (functions/constraint-fn 2))
  (put 'defmixin 'clojure-backtracking-indent '(4 (2)))
  (require 'clojure-mode-extra-font-locking))

(use-package cider
  :commands (cider-jack-in cider-jack-in-clojurescript)
  :load-path ("~/workspace/cider")
  :config
  ;; Enable eldoc in Clojure buffers
  (add-hook 'cider-mode-hook #'eldoc-mode)

  ;; ;; Disable showing eldoc, use lsp-mode.
  ;; (setq cider-eldoc-display-for-symbol-at-point nil)

  ;; Add Cider Xref backend to the end, use lsp-mode.
  ;; (setq cider-xref-fn-depth -90)
  ;; (setq cider-xref-fn-depth 0)
  ;; (setq cider-xref-fn-depth 90)

  ;; Pretty print in the REPL.
  (setq cider-repl-use-pretty-printing t)

  ;; Whether to automatically download source artifacts for 3rd-party
  ;; Java classes.
  (setq cider-download-java-sources t)

  ;; Hide *nrepl-connection* and *nrepl-server* buffers from appearing
  ;; in some buffer switching commands like switch-to-buffer
  (setq nrepl-hide-special-buffers nil)

  ;; Enabling CamelCase support for editing commands(like forward-word,
  ;; backward-word, etc) in the REPL is quite useful since we often have
  ;; to deal with Java class and method names. The built-in Emacs minor
  ;; mode subword-mode provides such functionality
  (add-hook 'cider-repl-mode-hook #'subword-mode)

  ;; Auto-select the error buffer when it's displayed:
  (setq cider-auto-select-error-buffer t)

  ;; Controls whether to stop a test run on failure/error.
  (setq cider-test-fail-fast nil)

  ;; Controls whether to pop to the REPL buffer on connect.
  (setq cider-repl-pop-to-buffer-on-connect nil)

  ;; T to wrap history around when the end is reached.
  (setq cider-repl-wrap-history t)

  ;; Don't log protocol messages to the `nrepl-message-buffer-name' buffer.
  (setq nrepl-log-messages t)

  ;; Don't show the `*cider-test-report*` buffer on passing tests.
  (setq cider-test-report-on-success nil)

  ;; (setq cider-injected-middleware-version "0.0.0")
  ;; (setq cider-required-middleware-version "0.0.0")

  ;; (add-to-list 'cider-jack-in-nrepl-middlewares "stem.nrepl/middleware")
  ;; (cider-add-to-alist 'cider-jack-in-dependencies "stem/nrepl" "1.1.2-SNAPSHOT")

  ;; (add-to-list 'cider-jack-in-nrepl-middlewares "nrepl-rebl.core/wrap-rebl")
  ;; (cider-add-to-alist 'cider-jack-in-dependencies "nrepl-rebl/nrepl-rebl" "0.1.1")

  ;; (cider-add-to-alist 'cider-jack-in-dependencies "refactor-nrepl/refactor-nrepl" "3.11.0")

  ;; TODO: How to do this without printing a message?
  (defun custom/cider-inspector-mode-hook ()
    (visual-line-mode -1)
    (toggle-truncate-lines 1))

  (add-hook 'cider-inspector-mode-hook #'custom/cider-inspector-mode-hook))

(use-package clj-refactor
  :hook ((clojure-mode . clj-refactor-mode))
  :config
  (cljr-add-keybindings-with-prefix "C-c C-R")
  ;; Don't place a newline after the `:require` and `:import` tokens
  (setq cljr-insert-newline-after-require nil)
  ;; Don't use prefix notation when cleaning the ns form.
  (setq cljr-favor-prefix-notation nil)
  ;; Don't warn when running an AST op.
  (setq cljr-warn-on-eval nil)
  ;; ;; Don't build AST on startup.
  (setq cljr-eagerly-build-asts-on-startup nil)
  ;; Print a message when the AST has been built.
  (setq cljr--debug-mode t))

(use-package codegpt
  :commands (codegpt))

(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c b" . consult-bookmark)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-project-imenu)
         ;; M-s bindings (search-map)
         ("M-s f" . consult-find)
         ("M-s L" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch)
         :map isearch-mode-map
         ("M-e" . consult-isearch)                 ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch)               ;; orig. isearch-edit-string
         ("M-s l" . consult-line))                 ;; required by consult-line to detect isearch

  ;; Enable automatic preview at point in the *Completions* buffer.
  ;; This is relevant when you use the default completion UI,
  ;; and not necessary for Selectrum, Vertico etc.
  ;; :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file
   :preview-key "M-.")

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root))

(use-package consult-gh
  :after consult)

(use-package geiser
  :commands (geiser run-geiser))

(use-package geiser-guile
  :after geiser
  :custom
  (geiser-default-implementation 'guile)
  :config
  ;; (add-to-list 'geiser-guile-load-path (expand-file-name "~/workspace/guix"))
  (add-to-list 'geiser-guile-load-path (expand-file-name "~/workspace/asahi-guix/installer/modules"))
  (add-to-list 'geiser-guile-load-path (expand-file-name "~/workspace/asahi-guix/channel/modules"))
  (add-to-list 'geiser-guile-load-path (expand-file-name "~/workspace/asahi-guix/website/modules"))
  (add-to-list 'geiser-guile-load-path (expand-file-name "~/workspace/asahi-guix/maintenance/modules"))
  (add-to-list 'geiser-guile-load-path (expand-file-name "~/workspace/guix-channel"))
  (add-to-list 'geiser-guile-load-path (expand-file-name "~/workspace/guix-home/modules"))
  (defun geiser-con--tq-filter (tq in)
    (when (buffer-live-p (tq-buffer tq))
      (with-current-buffer (tq-buffer tq)
        (if (tq-queue-empty tq)
            (progn (geiser-log--error "Unexpected queue input:\n %s" in)
                   (delete-region (point-min) (point-max)))
          (goto-char (point-max))
          (insert in)
          (goto-char (point-min))
          (when (re-search-forward (tq-queue-head-regexp tq) nil t)
            (unwind-protect
                (funcall (tq-queue-head-fn tq)
                         (tq-queue-head-closure tq)
                         (buffer-substring (point-min) (point)))
              (delete-region (point-min) (point-max))
              (tq-queue-pop tq))))))))

(use-package graphql-mode
  :mode "\\.graphql\\'"
  :config
  (setq graphql-url "http://localhost:7000/graphql"))

(defun guix-system-p ()
  (interactive)
  (file-exists-p "/run/current-system/configuration.scm"))

(defun guix-home-reconfigure ()
  "Run Guix Home reconfigure."
  (interactive)
  (with-current-buffer (get-buffer-create "*Guix Home Reconfigure*")
    (pop-to-buffer (current-buffer))
    (erase-buffer)
    (let ((default-directory "~/workspace/guix-home"))
      (async-shell-command "~/.config/guix/current/bin/guix home reconfigure -L modules modules/r0man/guix/home/systems/precision.scm" (current-buffer)))))

(defun guix-system-reconfigure ()
  "Run Guix System reconfigure."
  (interactive)
  (with-current-buffer (get-buffer-create "*Guix System Reconfigure*")
    (pop-to-buffer (current-buffer))
    (erase-buffer)
    (let ((default-directory "~/workspace/guix-home"))
      (cd (format "/sudo::%s" (expand-file-name default-directory)))
      (async-shell-command "~/.config/guix/current/bin/guix system reconfigure -L modules modules/r0man/guix/system/m1.scm --allow-downgrades --no-offload" (current-buffer)))))

(use-package guix-prettify
  :hook ((after-init . global-guix-prettify-mode)))

(setq user-full-name "r0man")

(use-package dart-mode
  :hook (dart-mode . flutter-test-mode))

(use-package data-debug
  :bind (("M-:" . data-debug-eval-expression)))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package docker
  :commands (docker))

(use-package eieio-datadebug
  :after (eieio))

(use-package emacs-lisp
  :bind (("C-c C-p " . pp-eval-last-sexp)
         ("C-c C-j " . pp-json-eval-last-sexp)))

(use-package emr
  :commands (emr-show-refactor-menu))

(use-package embark
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :custom-face
  (embark-keybinding ((t :inherit bold)))
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package erlang
  :mode (("\\.erl\\'" . erlang-mode)))

(use-package flutter
  :after dart-mode
  :bind (:map dart-mode-map ("C-M-x" . #'flutter-run-or-hot-reload))
  :custom (flutter-sdk-path "/opt/flutter"))

(use-package forge
  :after magit
  :commands (forge-pull))

(use-package gif-screencast
  :commands gif-screencast-start-or-stop
  ;; :bind ("<f9>" . gif-screencast-start-or-stop)
  :config
  (setq gif-screencast-scale-factor 1.0))
;; (with-eval-after-load 'gif-screencast
;;   (setq gif-screencast-scale-factor 1.0)
;;   (define-key gif-screencast-mode-map (kbd "<f8>") 'gif-screencast-toggle-pause)
;;   (global-set-key (kbd "<f9>") 'gif-screencast-start-or-stop))

(use-package guix
  :hook ((scheme-mode . guix-devel-mode)))

(use-package gptel
  :commands gptel
  :config
  (require 'gptel-integrations)
  (setq-default gptel-backend
                (gptel-make-openai
                 "NuLLM Open AI"
                 :stream t
                 :models '(o1-mini o3-mini o4-mini o1 o3 gpt-4.1-nano gpt-4.1-mini gpt-4.1)
                 :host "ist-prod-litellm.nullmplatform.com"
                 :key (lambda ()
                        (auth-source-pick-first-password :host "ist-prod-litellm.nullmplatform.com" :user "apikey"))))
  :custom
  (gptel-use-curl "~/.guix-profile/bin/curl"))

(setq history-delete-duplicates t)

(use-package htmlize
  :commands (htmlize-buffer htmlize-file))

(use-package hy-mode
  :mode (("\\.hy\\'" . hy-mode))
  :config
  (setq hy-indent-specform
        '(("for" . 1)
          ("for*" . 1)
          ("while" . 1)
          ("except" . 1)
          ("catch" . 1)
          ("let" . 1)
          ("if" . 1)
          ("when" . 1)
          ("unless" . 1)
          ("test-set" . 1)
          ("test-set-fails" . 1))))

(use-package image
  :defer t
  :custom
  (image-use-external-converter t))

(use-package lisp-mode
  :mode (("source-registry.conf" . lisp-mode)))

(setq my-auto-save-directory (concat user-emacs-directory "auto-save/"))

(setq auto-save-file-name-transforms `((".*" ,my-auto-save-directory t)))

(setq my-backup-directory (concat user-emacs-directory "backups/"))

(setq backup-directory-alist `((".*" . ,my-backup-directory)))

(setq backup-by-copying t)

(setq vc-make-backup-files nil)

(setq delete-old-versions t)

(setq kept-new-versions 20)

(setq kept-old-versions 20)

(setq version-control t)

(setq vc-handled-backends nil)

(setq message-log-max 10000)

(use-package mermaid-mode
  :mode ("\\.mermaid\\'" "\\.mmd\\'"))

(setq use-short-answers t)

(show-paren-mode 1)

(setq debug-on-error nil)

(setq inhibit-startup-message t)

(column-number-mode)

(display-time-mode 0)

(setq require-final-newline t)

(setq show-trailing-whitespace t)

(setq tab-always-indent 'complete)

(setq term-buffer-maximum-size (* 10 2048))

(setq browse-url-browser-function 'browse-url-xdg-open)

(define-globalized-minor-mode global-goto-address-mode goto-address-mode goto-address-mode)
(global-goto-address-mode)

(setq confirm-kill-processes nil)

(defun colorize-compilation-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))

(use-package compile
  :commands (compile)
  :custom
  ;; Auto scroll compilation buffer.
  (compilation-scroll-output 't)
  :config
  (add-hook 'compilation-filter-hook #'colorize-compilation-buffer))

(use-package css-mode
  :mode ("\\.css\\'" . css-mode)
  :custom
  (css-indent-offset 2))

(use-package scss-mode
  :mode (("\\.sass\\'" . scss-mode)
         ("\\.scss\\'" . scss-mode))
  :custom
  (scss-compile-at-save nil))

(use-package desktop
  :defer 5
  :config
  (desktop-save-mode 1)
  ;; Disable Verbose reporting of lazily created buffers.
  (setq desktop-lazy-verbose nil)
  ;; Always save desktop.
  (setq desktop-save t)
  ;; Load desktop even if it is locked.
  (setq desktop-load-locked-desktop t)
  ;; Number of buffers to restore immediately.
  (setq desktop-restore-eager 4)
  ;; Don't save some buffers.
  (setq desktop-buffers-not-to-save
        (concat "\\("
                "\\.bbdb|\\.gz"
                "\\)$")))

(use-package inf-lisp
  :commands (inferior-lisp)
  :custom
  (inferior-lisp-program "sbcl"))

(use-package dired
  :bind (("C-x C-d" . dired))
  :commands (dired)
  :custom
  ;; Try to guess a default target directory.
  (dired-dwim-target t)
  ;; Switches passed to `ls' for Dired. MUST contain the `l' option.
  (dired-listing-switches "-alh"))

(defun find-dired-clojure (dir)
  "Run find-dired on Clojure files."
  (interactive (list (read-directory-name "Find Clojure files in directory: " nil "" t)))
  (find-dired dir "-name \"*.clj\""))

(defun find-dired-elisp (dir)
  "Run find-dired on Elisp files."
  (interactive (list (read-directory-name "Find Elisp files in directory: " nil "" t)))
  (find-dired dir "-name \"*.el\""))

(defun dired-do-shell-command-in-background (command)
  "In dired, do shell command in background on the file or directory named on
 this line."
  (interactive
   (list (dired-read-shell-command (concat "& on " "%s: ") nil (list (dired-get-filename)))))
  (call-process command nil 0 nil (dired-get-filename)))

(use-package dired-x
  :after dired
  :bind (:map dired-mode-map
              ("&" . dired-do-shell-command-in-background))
  :custom
  (dired-guess-shell-alist-user
   '(("\\.mp4\\'" "mplayer")
     ("\\.mkv\\'" "mplayer")
     ("\\.mov\\'" "mplayer")
     ("\\.xlsx?\\'" "libreoffice"))))

(use-package elec-pair
  :hook (after-init . electric-pair-mode))

(use-package prog-mode
  :hook (emacs-lisp-mode . prettify-symbols-mode))

(use-package elint
  :commands (elint-initialize elint-current-buffer)
  :bind (:map emacs-lisp-mode-map
              ("C-c e E" . elint-current-buffer)))

(use-package emacs-lisp-mode
  :no-require t
  :mode ("\\.el\\'" "Cask")
  :bind (:map emacs-lisp-mode-map
              ("C-c C-k" . eval-buffer)
              ("C-c e c" . cancel-debug-on-entry)
              ("C-c e d" . debug-on-entry)
              ("C-c e e" . toggle-debug-on-error)
              ("C-c e f" . emacs-lisp-byte-compile-and-load)
              ("C-c e l" . find-library)
              ("C-c e r" . eval-region)))

(use-package ert
  ;; :no-require t
  ;; :mode ("\\.el\\'")
  :bind (:map emacs-lisp-mode-map
              ("C-c ," . ert)
              ("C-c C-," . ert)))

(use-package elisp-slime-nav
  :diminish
  :hook (emacs-lisp-mode . elisp-slime-nav-mode))

(use-package server
  :if window-system
  :defer 1
  :config
  (unless (server-running-p)
    (server-start)))

(use-package emms
  :commands (emms)
  :config
  (emms-all)
  (emms-default-players)
  (add-to-list 'emms-player-list 'emms-player-mpd)
  (condition-case nil
      (emms-player-mpd-connect)
    (error (message "Can't connect to music player daemon.")))
  (setq emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find)
  (setq emms-player-mpd-music-directory (expand-file-name "~/Music"))
  (load-if-exists "~/.emms.el"))

(use-package expand-region
  :bind (("C-c C-+" . er/expand-region)
         ("C-c C--" . er/contract-region)))

(use-package flycheck
  :hook ((after-init . global-flycheck-mode)))

(use-package flycheck-elsa
  :hook ((emacs-lisp-mode . flycheck-elsa-setup)))

(use-package git-email
  :commands (git-email-send-email git-email-format-patch))

(use-package github-browse-file
  :commands (github-browse-file github-browse-file-blame))

(use-package inspector
  :commands (inspector-inspect-expression
             inspector-inspect-last-sexp))

(use-package tree-inspector
  :commands (tree-inspector-inspect-expression
             tree-inspector-inspect-last-sexp))

(use-package isa
  :commands (isa)
  :if (file-directory-p "~/workspace/nu/isa.el/")
  :load-path "~/workspace/nu/isa.el/")

(use-package jira
  :custom
  (jira-api-version 3)
  (jira-base-url "https://nubank.atlassian.net")
  (jira-username "roman.scherer@nubank.com.br"))

(use-package jiralib2
  :after (ox-jira)
  :defer t)

(use-package kubel
  :commands (kubel))

(use-package kubernetes
  :bind (("C-x C-k s" . kubernetes-overview))
  :commands (kubernetes-overview))

(use-package kotlin-mode
  :mode ("\\.kt\\'" "\\.kts\\'" "\\.ktm\\'"))

(use-package magit
  :bind (("C-x C-g s" . magit-status))
  :config
  (setq magit-stage-all-confirm nil)
  (setq magit-unstage-all-confirm nil)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package mcp
  :commands (mcp-hub)
  :config
  (setq mcp-hub-servers
        `(("apistemic" . (:command "/bin/sh"
                                   :args ("-c"
                                          ,(concat "cd ~/workspace/nu/apistemic && "
                                                   "clojure -X:mcp :port 45525")))))))

(use-package emacs-mcp
  :commands (emacs-mcp-run-stdio))

(use-package nu
  :commands (nu nu-datomic-query nu-session-switch)
  :load-path ("~/workspace/nu/nudev/ides/emacs/"
              "~/workspace/nu/nudev/ides/emacs/test/")
  :config
  (require 'nu)
  (require 'nu-metapod)
  (require 'nu-datomic-query))

(use-package nu-tools-build
  :commands (nu-tools-build)
  :load-path ("~/workspace/nu/tools-build/"))

(use-package java-mode
  :config
  (setq c-comment-start-regexp "\\(@\\|/\\(/\\|[*][*]?\\)\\)")
  (modify-syntax-entry ?@ "< b" java-mode-syntax-table))

(use-package js
  :mode "\\.js\\'"
  :custom
  (js-indent-level 2))

(use-package jarchive
  :config
  (jarchive-setup)
  :after (eglot))

(use-package rcirc
  :commands (rcirc)
  :custom
  (rcirc-default-nick "r0man")
  (rcirc-default-user-name "r0man")
  (rcirc-default-full-name "r0man")
  (rcirc-server-alist '(("irc.libera.chat"
                         :channels ("#clojure" "#guix" "#spritely")
                         :encryption tls
                         :port 6697)))
  (rcirc-private-chat t)
  (rcirc-debug-flag t)
  :config
  (load-if-exists "~/.rcirc.el")
  (add-hook 'rcirc-mode-hook
            (lambda ()
              (set (make-local-variable 'scroll-conservatively) 8192)
              (rcirc-track-minor-mode 1))))

(use-package message
  :defer t
  :custom
  ;; Send mail via smtpmail.
  (message-send-mail-function 'smtpmail-send-it)
  :init
  ;; GPG sign messages
  (add-hook 'message-send-hook 'mml-secure-message-sign-pgpmime))

(use-package macrostep
  :commands (macrostep-expand)
  :bind (:map emacs-lisp-mode-map
              ("C-c m" . macrostep-expand)))

(use-package makem
  :load-path ("~/workspace/makem.sh")
  :commands (makem))

(use-package markdown-mode
  :mode "\\.md\\'"
  :custom
  (markdown-hide-urls t)
  :config
  (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode)))

(use-package marginalia
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :hook (after-init . marginalia-mode))

(use-package mu4e
  :commands mu4e
  :config
  (setq mu4e-maildir "~/Mail")

  ;; Make sure that moving a message (like to Trash) causes the
  ;; message to get a new file name.  This helps to avoid the
  ;; dreaded "UID is N beyond highest assigned" error.
  ;; See this link for more info: https://stackoverflow.com/a/43461973
  (setq mu4e-change-filenames-when-moving t)

  ;; Do not show colors in the HTML.
  (setq shr-use-colors nil)

  ;; Refresh mail every minute.
  (setq mu4e-update-interval (* 1 60))

  ;; The policy to determine the context when entering the mu4e main view.
  (setq mu4e-context-policy 'pick-first)

  (setq mu4e-bookmarks
        '((:name "Burning Swell"
                 :query "maildir:/burningswell/* AND NOT flag:list"
                 :key ?b)
          (:name "Nubank"
                 :query "maildir:/nubank/* AND NOT flag:list"
                 :key ?n)
          (:name "Guix Devel"
                 :query "list:guix-devel.gnu.org"
                 :key ?g)
          (:name "Guix Help"
                 :query "list:help-guix.gnu.org"
                 :key ?h)
          (:name "Unread messages"
                 :query "flag:unread AND NOT flag:trashed AND NOT list:itaipu.nubank.github.com"
                 :key ?u)
          (:name "Today's messages"
                 :query "date:today..now AND NOT list:itaipu.nubank.github.com"
                 :key ?t)
          (:name "Last 7 days"
                 :query "date:7d..now AND NOT list:itaipu.nubank.github.com"
                 ;; :hide-unread t
                 :key ?w)
          (:name "Messages with images"
                 :query "mime:image/*"
                 :key ?p)))

  (setq mu4e-contexts
        (list
         (make-mu4e-context
          :name "Burningswell"
          :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/burningswell" (mu4e-message-field msg :maildir))))
          :vars '((mu4e-drafts-folder . "/burningswell/[Gmail]/Drafts")
                  (mu4e-refile-folder . "/burningswell/[Gmail]/All Mail")
                  (mu4e-sent-folder . "/burningswell/[Gmail]/Sent Mail")
                  (mu4e-trash-folder . "/burningswell/[Gmail]/Trash")
                  (user-full-name . "Roman Scherer")
                  (user-mail-address . "roman.scherer@burningswell.com")))
         (make-mu4e-context
          :name "Nubank"
          :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/nubank" (mu4e-message-field msg :maildir))))
          :vars '((mu4e-drafts-folder . "/nubank/[Gmail]/Drafts")
                  (mu4e-refile-folder . "/nubank/[Gmail]/All Mail")
                  (mu4e-sent-folder . "/nubank/[Gmail]/Sent Mail")
                  (mu4e-trash-folder . "/nubank/[Gmail]/Trash")
                  (user-full-name . "Roman Scherer")
                  (user-mail-address . "roman.scherer@nubank.com.br"))))))

(use-package multi-vterm
  :bind (("C-x M" . multi-vterm)
         ("C-x m" . multi-vterm-next)
         ;; :map projectile-mode-map
         ;; ("C-c p m" . multi-vterm-projectile)
         ))

(use-package multiple-cursors
  :defer 1)

(use-package nucli
  :bind (("C-x N" . nucli))
  :commands (nucli)
  :load-path ("~/workspace/nu/nucli.el/src"
              "~/workspace/nu/nucli.el/test"))

(add-to-list 'default-frame-alist '(mouse-color . "white"))

(use-package savehist
  :hook (after-init . savehist-mode)
  :custom
  (savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
  (savehist-file "~/.config/emacs/savehist"))

(defun save-buffer-as-is ()
  "Save file \"as is\", that is in read-only-mode."
  (interactive)
  (if buffer-read-only
      (save-buffer)
    (read-only-mode 1)
    (save-buffer)
    (read-only-mode 0)))

(use-package sly
  :commands (sly))

(use-package scheme
  :mode (("\\.scm\\'" . scheme-mode))
  :custom
  (scheme-program-name "guile"))

(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

(setq sql-indent-offset 0)

(eval-after-load "sql"
  '(load-if-exists "~/.sql.el"))

(use-package sql-indent
  :hook (sql-mode . sqlind-minor-mode))

(use-package tramp
  :defer t
  :config
  (setq tramp-verbose 10)
  (tramp-set-completion-function
   "ssh"
   '((tramp-parse-shosts "~/.ssh/known_hosts")
     (tramp-parse-hosts "/etc/hosts")))
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (setq tramp-remote-path
        (append tramp-remote-path
                '("~/.guix-profile/bin"
                  "~/.guix-profile/sbin"
                  "/run/current-system/profile/bin"
                  "/run/current-system/profile/sbin"))))

(use-package uniquify
  :defer t
  :custom
  (uniquify-after-kill-buffer-p t)
  (uniquify-buffer-name-style 'post-forward-angle-brackets)
  (uniquify-ignore-buffers-re "^\\*")
  (uniquify-separator "|"))

(use-package openai
  :defer t
  :config
  (setq openai-key #'openai-key-auth-source))

(use-package openwith
  :hook ((after-init . openwith-mode))
  :config
  (setq openwith-associations
        (list
         (list (openwith-make-extension-regexp
                '("mpg" "mpeg" "mp3" "mp4"
                  "avi" "wmv" "wav" "mov" "flv"
                  "ogm" "ogg" "mkv"))
               "vlc"
               '(file))
         (list (openwith-make-extension-regexp
                '("doc" "xls" "ppt" "odt" "ods" "odg" "odp"))
               "libreoffice"
               '(file)))))

(use-package orderless
  :after (vertico)
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides
   '(;; (command (styles partial-completion))
     (file (styles basic partial-completion))
     ;; (project-file (styles orderless partial-completion))
     ;; (symbol (styles partial-completion))
     ;; (variable (styles partial-completion))
     )))

(use-package org-gcal
  :commands (org-gcal-fetch org-gcal-sync)
  :config
  (setq org-gcal-remove-api-cancelled-events t)
  (setq org-gcal-client-id "307472772807-cb0c244ep89qoec5sdu672st8funmqtr.apps.googleusercontent.com")
  (setq org-gcal-client-secret
        (auth-source-pick-first-password
         :host org-gcal-client-id
         :user "roman.scherer@nubank.com.br"))
  (setq org-gcal-fetch-file-alist '(("roman.scherer@nubank.com.br" .  "~/nubank-calendar.org")))
  (add-to-list 'org-agenda-files "~/nubank-calendar.org"))

(use-package org-jira
  :load-path ("~/workspace/org-jira")
  :commands (org-jira-get-issues)
  :config
  (make-directory org-jira-working-dir t)
  :custom
  (jiralib-url "https://nubank.atlassian.net")
  (org-jira-users '(("Roman Scherer" . "5d56c909acb80e0d853a95ca"))))

(use-package org
  :mode (("\\.org\\'" . org-mode)))

(use-package org-agenda
  :after (org)
  :custom
  (org-agenda-include-diary t))

(use-package ob
  :after (org)
  :custom
  (org-confirm-babel-evaluate
   (lambda (lang body)
     (not (member lang '("plantuml")))))
  :init
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((clojure . t)
     (gnuplot . t)
     (emacs-lisp . t)
     ;; (mermaid . t)
     (plantuml . t)
     (ruby . t)
     (shell . t)
     (sql . t)
     (sqlite . t))))

(use-package ob-clojure
  :after (ob)
  :custom
  (org-babel-clojure-backend 'cider))

(use-package org-plus-contrib
  :commands (org-invoice-report)
  :init (require 'org-invoice)
  :no-require t)

(use-package org-present
  :commands org-present
  :bind (:map org-present-mode-keymap
              ([left] . 'left-char)
              ([right] . 'right-char)
              (">" . 'org-present-next)
              ("<" . 'org-present-prev)))

(use-package ox-reveal
  :after (ox))

(use-package org-roam
  :after (org)
  :commands (org-roam-capture
             org-roam-node-find
             org-roam-node-insert)
  :config
  (org-roam-db-autosync-mode)
  :custom
  (org-roam-directory (file-truename "~/org-roam")))

(use-package org-tree-slide
  :bind
  (:map org-tree-slide-mode-map
        ("<prior>" . org-tree-slide-move-previous-tree)
        ("<next>" . org-tree-slide-move-next-tree))
  :config
  (add-hook 'org-tree-slide-mode-hook (lambda () (org-display-inline-images))))

(use-package ox-gfm
  :after (ox))

(use-package ox-jira
  :after (ox))

(use-package pandoc-mode
  :hook markdown-mode)

(use-package ox-pandoc
  :after (ox))

(use-package paredit
  :diminish
  ;; Bind RET to nil, to fix Cider REPL buffer eval issue
  :bind (:map paredit-mode-map ("RET" . nil))
  :hook ((cider-repl-mode . paredit-mode)
         (clojure-mode . paredit-mode)
         (clojurescript-mode . paredit-mode)
         (emacs-lisp-mode . paredit-mode)
         (hy-mode . paredit-mode)
         (lisp-mode . paredit-mode)
         (scheme-mode . paredit-mode)))

(use-package pass
  :commands (pass pass-copy))

(use-package pepita
  :commands (pepita-new-search)
  :config
  (setq pepita-splunk-url "https://localhost:8089/services/"))

(use-package pixel-scroll
  :hook (after-init . (lambda () (pixel-scroll-precision-mode 1))))

(use-package plantuml-mode
  :mode (("\\.plantump\\'" . plantuml-mode)
         ("\\.plu\\'" . plantuml-mode)
         ("\\.pum\\'" . plantuml-mode)
         ("\\.uml\\'" . plantuml-mode))
  :custom
  (org-plantuml-jar-path "~/.guix-profile/share/java/plantuml.jar"))

(use-package plz
  :defer t
  :config
  ;; When not on a Guix system, use curl from host
  (unless (file-exists-p "/run/current-system")
    (setq plz-curl-program "~/.guix-profile/bin/curl")))

(defun pp-json-display-expression (expression out-buffer-name)
  "Prettify and display EXPRESSION in an appropriate way, depending on length.
If a temporary buffer is needed for representation, it will be named
after OUT-BUFFER-NAME."
  (with-current-buffer (get-buffer-create out-buffer-name)
    (switch-to-buffer-other-window (current-buffer))
    (js-mode)
    (erase-buffer)
    (json-insert expression)
    (json-pretty-print-buffer)
    (beginning-of-buffer)))

(defun pp-json-eval-expression (expression)
  "Evaluate EXPRESSION and pretty-print its value.
Also add the value to the front of the list in the variable `values'."
  (interactive
   (list (read--expression "Eval: ")))
  (message "Evaluating...")
  (let ((result (eval expression lexical-binding)))
    (values--store-value result)
    (pp-json-display-expression result "*Pp JSON Eval Output*")))

(defun pp-json-eval-last-sexp (arg)
  "Run `pp-json-eval-expression' on sexp before point.
With ARG, pretty-print output into current buffer.
Ignores leading comment characters."
  (interactive "P")
  (if arg
      (insert (pp-to-string (eval (elisp--eval-defun-1
                                   (macroexpand (pp-last-sexp)))
                                  lexical-binding)))
    (pp-json-eval-expression (elisp--eval-defun-1
                              (macroexpand (pp-last-sexp))))))

(use-package projectile
  :bind
  (("C-x C-f" . projectile-find-file)
   :map projectile-command-map
   ("s g" . consult-grep)
   ("s r" . consult-ripgrep))
  :bind-keymap
  (("C-c p" . projectile-command-map))
  :custom
  (projectile-completion-system 'default)
  :config
  (add-to-list 'projectile-project-root-files-bottom-up "pubspec.yaml")
  (add-to-list 'projectile-project-root-files-bottom-up "BUILD")
  (projectile-register-project-type 'guix '("guix.scm")
                                    :project-file "guix.scm"
                                    :compile "make"
                                    :src-dir "modules/"
                                    :test "make test"
                                    :test-dir "tests/"
                                    :test-suffix "")
  :hook
  ((after-init . projectile-mode)))

(use-package ruby-mode
  :mode (("Capfile\\'" . ruby-mode)
         ("Gemfile\\'" . ruby-mode)
         ("Guardfile\\'" . ruby-mode)
         ("Rakefile\\'" . ruby-mode)
         ("Vagrantfile\\'" . ruby-mode)
         ("\\.gemspec\\'" . ruby-mode)
         ("\\.rake\\'" . ruby-mode)
         ("\\.ru\\'" . ruby-mode)))

(use-package rainbow-mode
  :defer 1)

(use-package redshank
  :diminish
  :hook ((emacs-lisp-mode . redshank-mode)
         (lisp-mode . redshank-mode)))

(use-package scala-mode
  :interpreter ("scala" . scala-mode)
  :mode "\\.scala\\'")

(use-package sbt-mode
  :commands (sbt-start sbt-command)
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (setq sbt:program-options '("-Dsbt.supershell=false")))

(use-package sendmail
  :defer t
  :custom
  ;; Send mail via smtpmail.
  (send-mail-function 'smtpmail-send-it))

(use-package show-fonts
  :commands (show-font-list show-font-select-preview))

(use-package simple
  :defer t
  :custom
  ;; Use mu4e to send emails.
  (mail-user-agent 'mu4e-user-agent))

(use-package slite
  :commands (slite-run))

(use-package emacs
  :custom
  ;; My email address.
  (user-mail-address "roman.scherer@burningswell.com"))

(use-package smtpmail
  :defer t
  :custom
  ;; Whether to print info in debug buffer.
  (smtpmail-debug-info t)
  ;; The name of the host running SMTP server.
  (smtpmail-smtp-server "smtp.gmail.com")
  ;; SMTP service port number.
  (smtpmail-smtp-service 587)
  ;; Type of SMTP connections to use.
  (smtpmail-stream-type 'starttls))

(global-so-long-mode 1)

(use-package paimon
  :commands (paimon)
  :load-path
  ("~/workspace/paimon.el/src"
   "~/workspace/paimon.el/test")
  :config
  (require 'nu-paimon))

(use-package lsp-mode
  :bind-keymap ("C-c l" . lsp-command-map)
  :commands (lsp)
  :load-path ("~/workspace/lsp-mode"
              "~/workspace/lsp-mode/clients")
  :hook (;; (clojure-mode . lsp-deferred)
         ;; (clojure-ts-mode . lsp-deferred)
         ;; (clojurec-mode . lsp-deferred)
         ;; (clojurescript-mode . lsp-deferred)
         (dart-mode . lsp-deferred)
         (elixir-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration)
         (lsp-mode . lsp-lens-mode)
         (scala-mode . lsp-deferred)
         ;; (sql-mode . lsp-deferred)
         (terraform-mode . lsp-deferred)
         (yaml-mode . lsp-deferred))
  :custom
  (lsp-eldoc-enable-hover nil)
  (lsp-enable-indentation nil)
  (lsp-elixir-server-command '("~/workspace/elixir-ls/release/language_server.sh"))
  (lsp-file-watch-threshold nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-keymap-prefix "C-c l")
  (lsp-log-io t)
  (lsp-modeline-code-actions-enable nil)
  (lsp-prefer-flymake nil)
  (lsp-restart 'ignore)
  (lsp-sqls-server "~/go/bin/sqls")
  (lsp-terraform-server "~/bin/terraform-lsp")
  (lsp-ui-doc-enable nil)
  (lsp-ui-sideline-enable nil))

(use-package lsp-dart
  :after lsp-mode
  :hook dart-mode
  :custom
  (lsp-dart-dap-flutter-hot-reload-on-save t)
  (lsp-dart-dap-flutter-hot-restart-on-save nil)
  (lsp-dart-flutter-widget-guides nil)
  (lsp-dart-sdk-dir "/opt/flutter/bin/cache/dart-sdk"))

(use-package lsp-treemacs
  :after lsp-mode
  :commands lsp-treemacs-errors-list)

(use-package lsp-metals
  :after lsp-mode
  :config (setq lsp-metals-treeview-show-when-views-received nil))

(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode)

(use-package hover
  :defer t)

(use-package posframe
  :defer t)

(use-package dap-mode
  :load-path ("~/workspace/dap-mode")
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode))

(use-package treemacs
  :defer t)

(use-package soundklaus
  :commands
  (soundklaus-activities
   soundklaus-connect
   soundklaus-my-favorites
   soundklaus-my-playlists
   soundklaus-my-tracks
   soundklaus-playlists
   soundklaus-tracks)
  :load-path
  ("~/workspace/soundklaus.el"
   "~/workspace/soundklaus.el/test"))

(use-package stem
  :commands (stem)
  :if (file-directory-p "~/workspace/nu/stem.el/")
  :load-path ("~/workspace/nu/stem.el/src/"
              "~/workspace/nu/stem.el/test/"))

(setq-default indent-tabs-mode nil)

(use-package terraform-mode
  :mode "\\.tf\\'")

(use-package transient
  :defer t)

(use-package unfill
  :commands (unfill-region unfill-paragraph unfill-toggle))

(use-package undo-tree
  :hook (after-init . global-undo-tree-mode)
  :custom
  (undo-tree-auto-save-history nil))

(use-package vertico
  :hook (after-init . vertico-mode)
  :custom
  (vertico-cycle t))

(use-package virtualenvwrapper
  :commands (venv-workon)
  :custom
  (venv-location "~/.virtualenv"))

(use-package vterm
  :commands (vterm)
  :custom
  (vterm-max-scrollback 100000))

(use-package warnings
  :defer t
  :custom
  (warning-minimum-level :emergency))

(use-package web-mode
  :mode (("\\.jsx\\'" . web-mode)
         ("\\.html\\'" . web-mode))
  :custom
  (web-mode-code-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-markup-indent-offset 2))

(use-package which-key
  :defer 1
  :diminish
  :config (which-key-mode))

(use-package whisper
  :bind ("C-s-r" . whisper-run)
  :config
  (when (file-exists-p "/usr/share/vulkan/icd.d/nvidia_icd.json")
    (setenv "VK_ICD_FILENAMES" "/usr/share/vulkan/icd.d/nvidia_icd.json"))
  :custom
  (whisper-install-directory "~/workspace/")
  (whisper-install-whispercpp 'manual)
  (whisper-language "en")
  (whisper-model "large-v3-turbo"))

(use-package winner
  :hook ((after-init . winner-mode)))

(use-package wsd-mode
  :mode "\\.wsd\\'")

(use-package x509-mode
  :commands
  (x509-viewasn1
   x509-viewcert
   x509-viewcrl
   x509-viewdh
   x509-viewkey))

(use-package yaml-mode
  :mode (("\\.yaml\\'" . yaml-mode)
         ("\\.yaml.tmpl\\'" . yaml-mode)
         ("\\.yml\\'" . yaml-mode)))

(use-package yasnippet
  :hook ((js-mode . yas-minor-mode)
         (js2-mode . yas-minor-mode)
         (ruby-mode . yas-minor-mode)
         (sql-mode . yas-minor-mode))
  :config
  (yas-reload-all))

(use-package yasnippet-snippets
  :after (yasnippet)
  :defer t
  :config
  (add-to-list 'yas-snippet-dirs "~/workspace/guix/etc/snippets/yas"))

;; (add-hook 'window-scroll-functions (lambda (window startp) (redraw-frame)))

(add-hook
 'after-init-hook
 (lambda ()
   ;; Load system specific config.
   (load-if-exists (concat user-emacs-directory system-name ".el"))

   ;; Load keyboard bindings.
   (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
   (global-set-key (kbd "C-c n") 'cleanup-buffer)
   (global-set-key (kbd "C-c r") 'rotate-buffers)
   (global-set-key (kbd "C-x C-b") 'list-buffers)
   (global-set-key (kbd "C-x C-o") 'delete-blank-lines)
   (global-set-key (kbd "C-x TAB") 'indent-rigidly)
   (global-set-key (kbd "C-x ^") 'enlarge-window)
   (global-set-key (kbd "C-x f") 'find-file)
   (global-set-key (kbd "C-x h") 'mark-whole-buffer)

   (define-key emacs-lisp-mode-map (kbd "C-c C-t t") 'buttercup-run-at-point)
   (define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)
   (define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol)))
