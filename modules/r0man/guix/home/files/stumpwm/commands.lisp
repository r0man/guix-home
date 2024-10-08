(in-package :stumpwm)

;;; COMMANDS

(defcommand delete-window-and-frame () ()
  "Delete the current frame with its window."
  (delete-window)
  (remove-split))

(defcommand chromium () ()
  "Run the chromium web browser."
  (run-shell-command "chromium"))

(defcommand emacs-client () ()
  "Run the emacs client."
  (run-shell-command "emacsclient --create-frame"))

(defcommand herd-status () ()
  "Run herd status and show the result."
  (message "~a" (run-shell-command "herd status" t)))

(defcommand disk-free () ()
  (message "~a" (run-shell-command "df --human-readable" t)))

(defcommand firefox () ()
  "Run the Firefox web browser."
  (run-shell-command "firefox"))

(defcommand librewolf () ()
  "Run the Librewolf web browser."
  (run-shell-command "librewolf"))

(defcommand icecat () ()
  "Run the Icecat web browser."
  (run-shell-command "icecat"))

(defcommand browser () ()
  "Run the web browser."
  (librewolf))

(defcommand htop () ()
  "Run htop."
  (run-or-raise "kitty --name HTop htop" '(:instance "HTop")))

(defcommand keyboard-backlight (on-or-off)
    ((:y-or-n " Turn Keyboard Backlight on? "))
  "Turn the keyboard light on or off."
  (run-shell-command (format nil "keyboard-backlight ~a" (if on-or-off "on" "off"))))

(defcommand logs () ()
  "Show the logs."
  (run-or-raise "kitty --name Logs -e sudo journalctl -f -a -n 100" '(:instance "Logs")))

(defcommand polybar () ()
  "Run the polybar status bar."
  (run-shell-command "start-polybar"))

(defcommand slack () ()
  "Run slack."
  (run-or-raise "slack --startup" '(:instance "Slack")))

(defcommand terminal () ()
  "Run the kitty terminal."
  ;; (hsplit)
  ;; (fnext)
  (run-shell-command "kitty"))

(defcommand terminal-raise () ()
  "Run or raise the kitty terminal."
  (run-or-raise "kitty --name Terminal" '(:instance "Terminal")))

;; (defcommand (fprev tile-group) () ()
;;   "Cycle through the frame tree to the prev frame."
;;   (focus-prev-frame (current-group)))

;; (defcommand file-manager () ()
;;   "Run or raise the file manager."
;;   (run-or-raise "nautilus" '(:class "Nautilus")))

;; (defcommand volume-lower () ()
;;   "Lower the volume."
;;   (message "Lower volume"))

;; (defcommand volume-raise () ()
;;   "Raise the volume."
;;   (message "Raise volume"))

;; (defcommand volume-toggle () ()
;;   "Toggle the volume."
;;   (message "Toggle volume"))

(defun shift-windows-forward (frames win)
  (when frames
    (let ((frame (car frames)))
      (shift-windows-forward (cdr frames)
                             (frame-window frame))
      (when win
        (pull-window win frame)))))

(defcommand rotate-windows () ()
  "Rotate windows"
  (let* ((frames (group-frames (current-group)))
         (win (frame-window (car (last frames)))))
    (shift-windows-forward frames win)))

(defcommand swap-windows () ()
  (let* ((group (current-group))
         (cur-frame (tile-group-current-frame group))
         (frames (group-frames group)))
    (if (eq (length frames) 2)
        (progn (if (or (neighbour :left cur-frame frames)
                       (neighbour :right cur-frame frames))
                   (progn
                     (only)
                     (vsplit))
                   (progn
                     (only)
                     (hsplit))))
        (message "Works only with 2 frames"))))

;; Rofi

(defun rofi (mode)
  (run-shell-command (concat "rofi -show " mode " -m -1")))

(defcommand rofi-run () ()
  (rofi "run -sidebar-mode"))

(defcommand rofi-window () ()
  (rofi "window"))

(defcommand rofi-windowcd () ()
  (rofi "windowcd"))

;; Slop

(defun executable-find (name)
  "Tell if given executable is present in PATH."
  (let ((which-out (string-trim '(#\  #\linefeed) (run-shell-command (concat "which " name) t))))
    (unless (string-equal "" which-out) which-out)))

(defun slop-get-pos ()
  (mapcar #'parse-integer (ppcre:split "[^0-9]" (run-shell-command "slop -f \"%x %y %w %h\"" t))))

(defun slop-or-float ()
  "Slop the current window or just float if slop cli not present."
  (if (executable-find "slop")
      (let ((window (current-window))
            (pos (slop-get-pos)))
        (float-window window (current-group))
        (float-window-move-resize window
                                  :x (nth 0 pos)
                                  :y (nth 1 pos)
                                  :width (nth 2 pos)
                                  :height (nth 3 pos)))
      (run-commands "float-this")))

(defcommand slop-this () () (slop-or-float))
