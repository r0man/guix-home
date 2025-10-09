;;; early-init.el --- The Early Init File. -*- lexical-binding: t; -*-

;;; Commentary:

;; This file is loaded before the package system and GUI is
;; initialized.

;;; Code:

;; Use low garbage collection threshold on startup.
(setq gc-cons-threshold 64000000)

;; Use higher garbage collection threshold after initialization.
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold 100000000)))

;;; early-init.el ends here
