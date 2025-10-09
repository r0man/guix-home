;;; init.el --- The Emacs init file. -*- lexical-binding: t; -*-

;;; Commentary:

;;; The Emacs init file

;;; Code:

(add-to-list 'default-frame-alist '(horizontal-scroll-bars . nil))
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))

(if (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; Load customization settings first.

(setq custom-file (expand-file-name "~/.emacs.d/custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; Load theme.

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

;; This file loads Org-mode and then loads the rest of our Emacs
;; initialization from Emacs lisp embedded in literate Org-mode files.

(setq package-archives
      '(("melpa" . "http://melpa.org/packages/")
        ("melpa-stable" . "http://stable.melpa.org/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/")))

(setq dotfiles-dir (file-name-directory (or (buffer-file-name) load-file-name)))

;; Load up all literate org-mode files in this directory
(mapc (lambda (org-filename)
        (let ((started-at (current-time)))
          (org-babel-load-file org-filename)
          (message "Loaded Org Babel file: %s (%.03fs)." org-filename
                   (float-time (time-since started-at)))))
      (directory-files dotfiles-dir t "\\.org$"))

;;; init.el ends here
