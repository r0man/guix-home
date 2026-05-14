# clog-gui — desktop-style windowing

`clog-gui` turns a browser tab into something that *feels* like a native desktop app: floating, draggable, resizable windows; a menu bar; modal dialogs; pinnable / always-on-top behaviour; toast notifications; popup browser tabs that look like child windows.

Use it when the app is GUI-shaped (an admin console, a builder, a multi-pane editor). For marketing pages or mobile-first sites, reach for clog-web instead.

## Setup

```lisp
(defpackage #:my-app
  (:use #:cl #:clog #:clog-gui)
  (:export start))

(in-package :my-app)

(defun on-new-window (body)
  (setf (title (html-document body)) "My App")
  (clog-gui-initialize body)              ; injects clog-gui CSS+JS
  (add-class body "w3-cyan")              ; pleasant desktop bg
  ;; ...build the menubar and windows here...
  )
```

`clog-gui-initialize` *must* run before any `create-gui-*` call. Forgetting it shows up as "elements appear but look wrong / can't be dragged."

## The menu bar

The menu is a sequence: `create-gui-menu-bar` → drop-downs → items. Idiomatic CLOG threads them through `let*` with one shadowed variable per row:

```lisp
(let* ((menu  (create-gui-menu-bar body))
       (tmp   (create-gui-menu-icon menu :on-click 'on-help-about))
       (file  (create-gui-menu-drop-down menu :content "File"))
       (tmp   (create-gui-menu-item file :content "New"   :on-click 'on-new))
       (tmp   (create-gui-menu-item file :content "Open"  :on-click 'on-open))
       (tmp   (create-gui-menu-item file :content "Quit"  :on-click 'on-quit))
       (win   (create-gui-menu-drop-down menu :content "Window"))
       (tmp   (create-gui-menu-item win :content "Maximize All"
                                        :on-click 'maximize-all-windows))
       (tmp   (create-gui-menu-item win :content "Normalize All"
                                        :on-click 'normalize-all-windows))
       (tmp   (create-gui-menu-window-select win))   ; lists open windows
       (tmp   (create-gui-menu-full-screen menu)))
  (declare (ignore tmp)))
```

Built-ins worth remembering:

- `create-gui-menu-icon` — usually an app logo, opens an About dialog.
- `create-gui-menu-window-select` — auto-populated list of currently open child windows; clicking one brings it forward.
- `create-gui-menu-full-screen` — toggle real fullscreen for the tab.
- `maximize-all-windows` / `normalize-all-windows` — bulk window controls, useful as menu items directly (they accept `obj` like a handler).

## Windows

`create-gui-window` returns a `clog-gui-window`. Its body is `window-content` — that's where you put widgets, not the window itself.

```lisp
(defun on-file-count (obj)
  (let ((win (create-gui-window obj :title "Count")))
    (dotimes (n 100)
      (create-div (window-content win) :content n))))
```

Useful keyword args:

| Keyword | Effect |
|---|---|
| `:title` | titlebar text |
| `:content` | raw HTML body (alternative to building it after creation) |
| `:top` / `:left` / `:width` / `:height` | initial geometry, pixels |
| `:has-pinner` | show the "pin" icon on the titlebar |
| `:keep-on-top` | start pinned above other windows |
| `:hidden` | create invisible, set `(setf (visiblep w) t)` after configuring |
| `:client-movement` | use CSS transforms for drag (faster on weak hardware) |

Useful operations:

```lisp
(window-maximize win)       (window-normalize win)
(window-center   win)       (window-toggle-maximize win)
(window-to-top   win)       ; bring forward
(setf (window-title win) "...")
(set-on-window-can-size  win (lambda (w) ...))   ; veto a resize
(set-on-window-can-close win (lambda (w) ...))   ; veto a close
(set-on-window-close     win (lambda (w) ...))   ; cleanup hook
(current-window obj)        ; the focused gui window, or nil
```

`set-on-before-unload (window body) (lambda (obj) "")` returning a non-empty string asks the browser to confirm before the user closes the tab. Empty string = no prompt.

## Dialogs

clog-gui ships a small set of modal dialogs. They all take an `obj` (any clog-obj on the current connection) and a continuation that fires with the user's answer.

```lisp
(alert-dialog   obj "Hello, world")

(confirm-dialog obj "Shall we play a game?"
                (lambda (yes?) (if yes? ... ...))
                :ok-text "Yes" :cancel-text "No")

(input-dialog   obj "Name?"
                (lambda (text) (alert-dialog obj (format nil "Hi ~A" text))))

(prompt-dialog  obj
                (lambda (color) (alert-dialog obj color))
                :title "Pick a color"
                :completion (lambda (str)
                              (remove-if-not (curry #'search str) palette))
                :validation (lambda (str) (find str palette :test #'equal)))

(server-file-dialog obj "Pick a file" "./"
                    (lambda (path) (load-and-show path)))
```

`form-dialog` deserves its own line. You hand it a spec (list of field descriptions) and it builds an HTML form, lays it out, validates, and calls your continuation with an alist of submitted values:

```lisp
(form-dialog obj "Please enter your information."
  '(("Title"     "title"     :select (("Mr." "mr")
                                      ("Mrs." "mrs" :selected)
                                      ("Other" "other")))
    ("Eye Color" "color"     :radio  (("Blue" "blue")
                                      ("Brown" "brown" :checked)
                                      ("Green" "green")))
    ("Newsletter" "newsletter" :checkbox t)
    ("Name"      "name"      :text "Real Name")
    ("Email"     "email"     :email)
    ("Address"   "address"))
  (lambda (results)
    (when results              ; nil if user cancelled
      (alert-dialog obj (princ-to-string results))))
  :height 550)
```

Field types: `:text`, `:email`, `:password`, `:number`, `:date`, `:color`, `:select` (with list of option triples), `:radio` (same shape), `:checkbox`. `:default` sets an initial value where applicable.

## Toasts

Non-modal status messages that slide in from the corner. Use them when a dialog would be too heavy.

```lisp
(alert-toast obj "Saved"   "Your changes are persisted."
             :color-class "w3-green" :time-out 3)
(alert-toast obj "Warning" "Check your network."
             :color-class "w3-yellow" :time-out 5)
(alert-toast obj "Error"   "Connection lost."
             :color-class "w3-red")           ; no time-out -> stays until X
```

`:color-class` accepts any w3.css color class. `:time-out` is seconds; omit for a sticky toast that the user must dismiss.

## Popup browser tabs

`enable-clog-popup` (call once after `clog-gui-initialize`) lets you open a new browser tab as if it were a child window, with the same Lisp connection:

```lisp
(defun on-file-pop-tab (obj)
  (let ((pop (open-clog-popup obj)))
    (cond (pop (clog-gui-initialize pop)
               (add-class pop "w3-cyan")
               (create-div pop :content "I am a popup tab"))
          (t   (alert-toast obj "Popup failure" "Blocked by browser")))))

;; or a real OS window:
(open-clog-popup obj :specs "width=320,height=320")
```

`pop` is itself a `clog-body` — same API as the parent. Pop-up blockers will return `nil`; always check.

## Patterns

**One handler builds one window.** Define `on-file-XXX` per menu item; let the handler `create-gui-window` and populate it. Avoid stuffing UI construction inside menu-bar binding chains — it's harder to test.

**Window state belongs in the window.** If three handlers need to coordinate, attach the data to the window itself via `connection-data-item` keyed on `(format nil "win-~A" (html-id win))`, or stash plain CLOS-slot data on a subclass of `clog-gui-window`.

**Maximised is a UX hint.** Don't force `window-maximize` on first open — users with multiple windows expect to pile them up. Reserve it for single-document apps.

**Don't fight the modal stack.** Dialogs queue: another `alert-dialog` while one is open will stack on top. The user must clear them in LIFO order. If that's not what you want, gate the call on `(not (current-window obj))` or your own "dialog open?" flag.

## A complete reference example

Tutorial 22 in the upstream repo is the canonical "kitchen sink" for clog-gui — menubars, every dialog type, drawing on a canvas, embedding an iframe, toasts, popups, About box. Read it once end-to-end before designing a non-trivial clog-gui app; it answers most questions about idiomatic structure.
