# Upstream tutorials — index

CLOG ships 35 progressive tutorial files in `tutorial/01-tutorial.lisp` … `tutorial/35-tutorial.lisp` in the upstream repo (https://github.com/rabbibotton/clog). When you hit an edge this skill doesn't cover, the tutorial of the same theme is usually the fastest answer — they are small, self-contained, and run as-is via `(asdf:load-system :clog/tutorials)` plus `(clog-tut-NN:start-tutorial)`.

| # | Topic | When to read it |
|---:|---|---|
| 01 | Hello World | First exposure. |
| 02 | Closures in CLOG | How handlers capture per-tab state without `connection-data-item`. |
| 03 | Events fire in parallel | Mental model of CLOG's threading. |
| 04 | Reusing event handlers | Multi-target handlers, `target` dispatch. |
| 05 | `connection-data-item` | Per-tab key/value store; the canonical "shared state" answer. |
| 06 | Tasking and events | Long-running loops inside handlers; `validp` discipline. |
| 07 | First video game | Mini real-time loop, keyboard input. |
| 08 | Mice love containers | Drag/drop within a `clog-div`; `set-on-mouse-*`. |
| 09 | Tabs, panels, forms | The same UI tutorial 33 rewrites declaratively. |
| 10 | Canvas | 2D drawing API (`create-context2d`, `fill-rect`, paths). |
| 11 | Attaching to existing HTML | `attach-as-child` against server-rendered or hand-written pages. |
| 12 | Routing (CLOG as a website) | `set-on-new-window :path`, multi-route setup, SEO via `:boot-function`. |
| 13 | Flying solo (minimal project) | The smallest standalone `.asd` + `.lisp` template. |
| 14 | Local & session storage | Browser-side persistence; complementary to `connection-data-item`. |
| 15 | Multimedia | `create-video`, `create-audio`. |
| 16 | Bootstrap 4 | Bundling a third-party CSS/JS framework. |
| 17 | W3.CSS layout + form submit | Foundations of `clog-web`. |
| 18 | Drag and drop | The HTML5 DnD API surfaced via CLOG. |
| 19 | Using JavaScript components | `js-execute` / `js-query` patterns. |
| 20 | New plugin from a JS component | Wrapping a JS library as a CLOG class. |
| 21 | New plugin in Common Lisp | The Lisp-only plugin pattern (covered in `plugins-and-extension.md`). |
| 22 | clog-gui kitchen sink | Menubars, every dialog type, popups, toasts — covered in `clog-gui.md`. |
| 23 | Semaphores | Blocking a handler until another event fires. |
| 24 | clog-web containers | Sidebars, compositor, grid — covered in `clog-web.md`. |
| 25 | "Local" web app with clog-web | Single-screen app dressed as a webpage. |
| 26 | A clog-web page with forms | Form submit, validation patterns under clog-web. |
| 27 | Panel-box layouts | Composable region layouts. |
| 28 | CLOG Builder hello | Using the GUI builder. |
| 29 | Presentations (jQuery) | `clog-presentations` — bind a Lisp object to a CLOG object. |
| 30 | Instant websites — `clog-web-site` | Themed multi-page sites. |
| 31 | Database + auth — `clog-web-dbi`, `clog-auth` | End-to-end user-account site, covered in `db-and-deploy.md`. |
| 32 | DB-managed content websites | CMS-style content tables. |
| 33 | **`with-clog-create`** | **The declarative macro for building UIs — recommended over `let* (create-…)` chains; rewrites tutorial 9 declaratively.** |
| 34 | 2D WebGL | `clog-webgl`. |
| 35 | 3D WebGL | More WebGL. |

## Other upstream docs worth knowing

| File | What's in it |
|---|---|
| `README.md` | Top-level intro, install paths, screenshots. |
| `LEARN.md` | Reading order for Lisp + CLOG newcomers. |
| `CONCEPT.md` | Architectural sketch — server/client roles, connection model. |
| `MANUAL.md` | The authoritative API reference. Sectioned by clog-base, clog-window, clog-document, clog-element, clog-form, clog-canvas, clog-gui, clog-web, clog-data, clog-auth. When you need to know "does this accessor exist?", this is faster than grepping source. |
| `WEBSERVER.md` | Production deployment under Apache, including the systemd snippet and SSL termination. |
| `LINUX.md` / `MACOS.md` / `WINDOWS.md` / `ANDROID-TERMUX.md` / `NATIVE.md` | Platform-specific installation + packaging notes. |
| `QLOT.md` / `OCICL.md` / `VSCODE.md` | Workflow notes for specific tooling. |

## Running a tutorial

```lisp
(ql:quickload :clog/tutorials)
(in-package :clog-tut-22)        ; pick any of 01–35
(start-tutorial)
```

`start-tutorial` calls `initialize` and `open-browser`; close the tab and run `(clog:shutdown)` between tutorials to free the port. The tutorial sources are small and worth reading top-to-bottom — most CLOG patterns are illustrated by *one* of them.
