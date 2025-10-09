;;; early-init.el --- The Early Init File. -*- lexical-binding: t; -*-

;;; Commentary:

;; This file is loaded before the package system and GUI is
;; initialized.

;;; Code:

;; Disable GUI elements early
(add-to-list 'default-frame-alist '(horizontal-scroll-bars . nil))
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))

(if (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; Load customization settings first
(setq custom-file (expand-file-name "~/.emacs.d/custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; Use low garbage collection threshold on startup
(setq gc-cons-threshold 64000000)

;; Use higher garbage collection threshold after initialization
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold 100000000)))

;;; early-init.el ends here
