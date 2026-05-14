---
name: clog
description: Expert knowledge for building web-based GUIs and websites with CLOG (Common Lisp Omnificent GUI), with the declarative `with-clog-create` macro as the recommended way to build UIs. Use this skill whenever the user mentions CLOG, wants to build a Common Lisp web app or browser-rendered desktop-style GUI, asks about initialize/on-new-window/connection-data-item/clog-gui/clog-web/with-clog-create, needs help with WebSocket-driven Lisp UIs, writes a CLOG plugin or new element class, hooks CLOG into a SQL database (clog-dbi / clog-auth), or deploys a CLOG server behind a reverse proxy. Trigger even when the user just says "browser GUI in Common Lisp" or "web UI for my Lisp app" without naming CLOG by name.
---

# CLOG — The Common Lisp Omnificent GUI

CLOG paints native-feeling GUIs and full websites *from the server*. Common Lisp owns the state; the browser is a thin renderer. The wire is a WebSocket carrying JavaScript fragments that mutate the DOM and event payloads that flow back. From a CLOG programmer's point of view there is no front end — you build the UI tree by calling `create-*` functions on Lisp objects, you set its properties with `setf`, and you bind handlers with `set-on-*`. The DOM and JS are an implementation detail you usually never touch.

## The mental model

```
HTTP boot page ──► loads boot.js ──► opens WebSocket
                                          │
                                          ▼
              (CLOG server) ◄──── connection ────► (browser DOM)
                  │                                       ▲
                  │  (create-*, setf, set-on-*)           │
                  ▼                                       │
              clog-obj tree ──► JS fragments ─────────────┘
              (one per tab)                ◄── event payloads
```

A few invariants follow:

- **Every browser tab is a *connection***. CLOG calls `on-new-window` with a fresh `body` (a `clog-body`, root of that tab's tree). State is per-connection unless you put it somewhere global.
- **Events fire in parallel, one thread per event**. Inside a handler you can `sleep`, hit the DB, recurse, or sit in a `loop` — you're not blocking the world. But two handlers can run concurrently on the same connection, so guard shared mutable state.
- **`connection-data-item` is the per-tab key/value store**. Thread-safe, lives as long as the connection. Use it for "the live counter widget", "the current user", "the DB handle for this session", etc.
- **All UI updates are SETF**. `(setf (color elem) :red)`, `(setf (text elem) "hi")`, `(setf (hiddenp elem) t)`. There's no virtual DOM, no diff — you describe the change and it goes over the wire.
- **CLOG owns the boot page**. You don't write HTML by hand for the entry point unless you want to (`:boot-file` lets you). The default `/boot.html` opens the WebSocket and waits for the server to build the page.

Internalising this is the whole game. When in doubt, ask: *which connection?* and *which thread?*

## Minimum viable app

```lisp
(defpackage #:my-app
  (:use #:cl #:clog)
  (:export start))

(in-package :my-app)

(defun on-new-window (body)
  "Called once per browser tab. BODY is the root clog-obj for this connection."
  (setf (title (html-document body)) "My App")
  (with-clog-create body
      (h1 (:bind hello :content "Hello, click me!"))
    (set-on-click hello
                  (lambda (obj)
                    (declare (ignore obj))
                    (setf (color hello) :green)))))

(defun start ()
  (initialize #'on-new-window)   ; starts the server on :8080
  (open-browser))                ; pops a tab at http://127.0.0.1:8080/
```

That's it. `initialize` boots the web server and waits; `on-new-window` is fired for every visiting tab. To stop the server: `(shutdown)`.

`with-clog-create` is the recommended way to build UIs in CLOG — it's a declarative macro that mirrors the DOM tree and pulls out variable bindings via `:bind`, replacing the deeply-nested imperative chains of `create-*` calls that real apps otherwise grow. See ["Declarative GUI construction"](#declarative-gui-construction) below; reach for `with-clog-create` whenever you're building more than two or three widgets.

## The four families of functions

Almost every CLOG call falls into one of four buckets. Once you spot the pattern, the API is small.

| Family | Shape | What it does |
|---|---|---|
| **Constructors** | `(create-XXX parent &key content class html-id ...)` → element | Build a DOM child and return its CLOG object. `create-div`, `create-section`, `create-button`, `create-form`, `create-canvas`, `create-img`, … |
| **Accessors** | `(prop element)` / `(setf (prop element) v)` | Read or mutate a property: `color`, `text`, `hiddenp`, `width`, `inner-html`, `value`, `place-holder`, `editablep`, … |
| **Event binders** | `(set-on-XXX element handler)` | Register a handler. `set-on-click`, `set-on-submit`, `set-on-change`, `set-on-mouse-down`, `set-on-key-press`, … |
| **Window/document** | `(window body)`, `(html-document body)`, `(location body)` | Reach the per-tab `clog-window`, `clog-document`, `clog-location` for global things (alerts, log, scroll, URL parsing). |

If you need something CLOG doesn't wrap, `(js-execute obj "any.javascript.here")` and `(js-query obj "expr")` are the escape hatches.

## State and threading

Per-tab state — including widget references you need from multiple handlers — goes through `connection-data-item`:

```lisp
(defun on-new-window (body)
  ;; stash the element so other handlers can mutate it
  (setf (connection-data-item body "counter")
        (create-section body :h2 :content "0"))
  (let ((btn (create-button body :content "+1")))
    (set-on-click btn
                  (lambda (obj)
                    (let ((c (connection-data-item obj "counter")))
                      (setf (text c)
                            (princ-to-string
                              (1+ (parse-integer (text c)))))))))) 
```

Long-running work is just a `loop` inside the handler — CLOG already gave you a dedicated thread. The idiom for "start/stop" is a flag in `connection-data-item`, checked with `validp` to bail when the tab closes:

```lisp
(defun blink (obj)
  (unless (connection-data-item obj "running")
    (setf (connection-data-item obj "running") t)
    (loop while (and (validp obj) (connection-data-item obj "running")) do
      (setf (color obj) :green) (sleep 0.3)
      (setf (color obj) :red)   (sleep 0.3))))
```

`validp` is your friend whenever a handler outlives a single tick — without it, the loop keeps running after the user closes the tab.

For data shared *between* connections (a chat room, a live ticker), use ordinary Lisp globals, but protect them — `bordeaux-threads:make-lock` and `with-lock-held` are fine. CLOG itself imposes no concurrency model on your domain state.

## Routing — CLOG as a website

A CLOG app starts as a one-page GUI but can serve a whole site. `initialize` registers the default handler; `set-on-new-window` registers one per URL path:

```lisp
(defun start ()
  (initialize 'on-home
              :static-root #P"/var/www/static/"  ; served at /
              :extended-routing t)               ; allow arbitrary paths
  (set-on-new-window 'on-about    :path "/about")
  (set-on-new-window 'on-post     :path "/post" :boot-file "/post.html")
  (set-on-new-window 'on-fallback :path :default))
```

Inside a handler, `(path-name (location body))` gives you the actual URL the user requested — useful when one handler serves many paths under `:extended-routing`.

`:boot-file` lets a route start from an existing HTML page (with `boot.js` linked in). You can then `attach-as-child` to reach elements that were rendered server-side or by another generator — see tutorial 12 for a full example.

## Choosing a layer

CLOG ships three coherent UI layers on top of the same primitives. Pick one per app — they compose but the conventions differ.

| Layer | When | Init call | Read |
|---|---|---|---|
| **clog-base** (this file) | Simple pages, one-screen tools, anything where you want to drive the DOM directly. | none (already in `:use #:cl #:clog`) | examples above |
| **clog-gui** | Desktop-style windowing: floating windows, menubars, dialogs, modal forms, toasts. Feels like a native app inside the browser. | `(clog-gui-initialize body)` after setting title | [`references/clog-gui.md`](references/clog-gui.md) |
| **clog-web** | Responsive websites: sidebars, grids, panels, hero sections, mobile layouts. Uses w3.css under the hood. | `(clog-web-initialize body)` | [`references/clog-web.md`](references/clog-web.md) |

The clog-gui and clog-web initialize calls add CSS/JS the layer needs and have to run before you create that layer's widgets. The packages are `clog-gui` and `clog-web` respectively — `:use` them alongside `:cl` and `:clog`.

If a request mixes both ("a desktop window containing a responsive form"), you can `clog-web-initialize` inside a `window-content` panel. The two layers don't fight.

## Declarative GUI construction

**Strong recommendation: build UIs with `with-clog-create`, not chains of `create-*` calls.** The imperative chain is fine for the one-element examples above, but anything larger collapses into a wall of `let*` bindings with `tmp` shadowing, where the shape of the resulting DOM is impossible to see at a glance. `with-clog-create` (in `:clog`, contributed by Mariano Montone, see upstream `tutorial/33-tutorial.lisp`) makes the source code mirror the DOM tree, which is what you actually wanted:

```lisp
(with-clog-create body
    (div (:bind page :class "wrap")
         (h1 (:content "Profile"))
         (form (:bind f)
               (form-element (:bind name :text :label (create-label f :content "Name:")))
               (br ())
               (form-element (:submit :value "Save"))))
  (set-on-submit f
                 (lambda (obj)
                   (declare (ignore obj))
                   (alert (window body) (value name)))))
```

How the macro reads:

- The first symbol after a `(...)` form names the CLOG element type — it expands to `create-<type>`. So `div` → `create-div`, `form-element` → `create-form-element`, `h1` → `create-section :h1`. Almost every `create-*` in CLOG has a matching keyword here.
- Keyword args inside the `(...)` are forwarded to the constructor (`:content`, `:class`, `:bind`, plus type-specific keys like `:value`, `:label`, `:multiple`).
- `(:bind sym)` makes the element available as a lexical variable `sym` *inside the body following the tree*. That body — the trailing forms after the last nested element — runs with every `:bind` in scope, which is where you wire handlers, mutate properties that depend on siblings, and do any setup that the constructor keywords couldn't express.
- Indentation = DOM nesting. Each nested form is a child of the form that contains it. There is no separate "parent argument" to track.

When to use what:

- **`with-clog-create`** — anything that builds more than two or three related elements, especially forms, panels, side-by-side layouts, tabs, anything with structure. This is the *default*. Read tutorial 33 for the full vocabulary (`form-element` types, `:label`, `:label-for`, `select`/`option`/`optgroup`, `fieldset`, etc.).
- **Bare `create-*` calls** — single-element insertions inside an event handler ("on click, append a `<div>`"), or when you genuinely want imperative control flow that the macro doesn't model (e.g. `dotimes` building rows). For one-offs the macro is overkill.
- **Both** — `with-clog-create` and `create-*` coexist freely. You can mix them when a subtree is computed dynamically.

Wrap the macro body in `with-connection-cache` when you set many properties in a row — it batches the wire traffic so the browser sees one update instead of N:

```lisp
(with-connection-cache (body)
  (with-clog-create body
      (div ()
           (h1 (:bind title :content "Loading…"))
           (div (:bind status)))
    (setf (color title) :gray)
    (setf (text status) "Fetching data…")))
```

Without `with-connection-cache`, each `setf` is its own round-trip; with it, the macro buffers them and sends one packet at the end. For static, server-rendered pages this is invisible; for screens that mutate dozens of properties on first paint, it's the difference between snappy and visibly slow.

## Talking to the browser when CLOG doesn't have a wrapper

```lisp
(js-execute obj "console.log('hi from lisp')")           ; fire-and-forget
(js-query   obj "navigator.userAgent")                   ; -> string back
(create-child body "<svg viewBox='...'>…</svg>")         ; inject raw HTML
(attach-as-child body "existing-id")                     ; adopt a DOM node
```

`attach-as-child` is the key to "I have an existing HTML page, make it dynamic" — give CLOG the `id` and it returns a `clog-obj` you can `set-on-click` like anything else.

## What lives elsewhere

- **Desktop-style windows, menus, dialogs, toasts, popups** → [`references/clog-gui.md`](references/clog-gui.md).
- **Responsive containers, grids, sidebars, compositors, panels** → [`references/clog-web.md`](references/clog-web.md).
- **Defining a new CLOG element class** (your own component, exported as `create-foo`) → [`references/plugins-and-extension.md`](references/plugins-and-extension.md).
- **Database binding (`clog-dbi`), auth (`clog-auth`), production deployment behind nginx/Apache, server keyword args** → [`references/db-and-deploy.md`](references/db-and-deploy.md).
- **The full tutorial line-up (35 progressive examples in `tutorial/` in the upstream repo)** is the best reference when you hit an edge — index in [`references/tutorials-index.md`](references/tutorials-index.md).

## Gotchas the API doesn't warn you about

- **`initialize` blocks the calling thread inside SBCL only if you ask it to.** In a REPL session it returns immediately and the server runs in background threads — fine for development. In a script (`sbcl --script`, systemd `ExecStart`) you need a `(loop (sleep 360))` or `(bt:join-thread …)` keep-alive at the end or the process exits and takes the server with it.
- **`open-browser` is for development.** Production never wants it — drop the call, deploy headless, let users hit the URL.
- **`set-on-click` replaces the previous handler.** There is no "add". If you need many handlers on one event, dispatch from a single handler. (Or use `set-on-event` / `set-on-event-with-data` for the lower-level interface that supports cancellation.)
- **`(setf (inner-html elem) "...")` blows away child `clog-obj` references.** The DOM update is fine but any Lisp variables pointing at the discarded children are now dangling — `validp` will return nil. Prefer `create-*` and surgical `setf (text x)` for stuff you intend to keep handles to.
- **WebSocket security: server and page must share an origin.** When deploying behind a reverse proxy, terminate TLS and forward both HTTP and WS on the *same* hostname/port; mixed origins will silently fail to upgrade.
- **`color`, `background-color`, `text` etc. accept keywords *or* strings.** `:red` works; `"red"` works; `"#ff0000"` works. Don't over-quote.
- **`declare (ignore obj)`** — most `set-on-*` handlers take the element as their first arg even when you don't need it. Idiomatic CLOG just declares it ignored rather than using `_`-style.

## Quick API recall

When stuck, this trio answers most questions:

```lisp
(describe (window body))   ; the clog-window for this tab
(describe body)            ; the clog-body — your root
(apropos "create-" :clog)  ; every constructor
```

The MAGICL/CLOG style of "everything is a generic function on a CLOS object" means SLIME/Sly autocompletion of `(create-` or `(set-on-` will reveal the API quickly. The official MANUAL.md in the upstream repo is the authoritative reference; if a function name surprises you, `M-.` to find its source — CLOG's Lisp is straightforward and worth reading.

## Idiomatic snippets

**Form submit → DB write → success message** (declarative — preferred)
```lisp
(with-clog-create body
    (form (:bind form)
          (form-element (:bind email :email :label (create-label form :content "Email:")))
          (br ())
          (form-element (:bind name  :text  :label (create-label form :content "Name:")))
          (br ())
          (form-element (:submit :value "Save")))
  (set-on-submit form
                 (lambda (obj)
                   (declare (ignore obj))
                   (save-user (value email) (value name))
                   (setf (hiddenp form) t)
                   (create-div body :content "Saved!" :class "w3-pale-green"))))
```

**Live updates from a background thread**
```lisp
(bt:make-thread
  (lambda ()
    (loop while (validp counter) do
      (sleep 1)
      (setf (text counter)
            (princ-to-string (incf (connection-data-item body "n")))))))
```

**Toast notification (with clog-gui loaded)**
```lisp
(clog-gui:alert-toast body "Saved" "Your changes are persisted."
                      :color-class "w3-green" :time-out 3)
```

**Modal confirm dialog**
```lisp
(clog-gui:confirm-dialog body "Delete this row?"
  (lambda (yes?) (when yes? (delete-row id)))
  :ok-text "Delete" :cancel-text "Keep")
```

These four cover ≈ 80% of real CLOG app code. Once you can read them at a glance, the rest of the framework is just naming.
