# clog-web — responsive web layouts

`clog-web` is CLOG's "build a website" layer. It wraps w3.css and adds Lisp-side constructors for containers, grids, sidebars, panels, compositors, code blocks, and a few opinionated form/auth helpers. Output is mobile-responsive by default — the same page looks correct on a phone, a laptop, and a 4K monitor.

Use it for marketing pages, dashboards, blogs, CMS-style sites, anything that's "page-shaped" rather than "window-shaped". For floating, draggable windows, use `clog-gui` instead (the two layers compose if you really need both).

## Setup

```lisp
(defpackage #:my-site
  (:use #:cl #:clog #:clog-web)
  (:export start))

(in-package :my-site)

(defun on-new-window (body)
  (clog-web-initialize body)            ; loads w3.css and clog-web JS
  (setf (title (html-document body)) "My Site")
  ;; ...build the page...
  )
```

`clog-web-initialize` must run before any `create-web-*` call. By default it also adds a default theme; pass `:w3-css-url nil` if you want to load your own CSS instead.

## Container types

`clog-web` widgets compose into a tree. Each container is a `create-web-XXX` returning a CLOG element you put further widgets in.

| Constructor | What it is | Typical placement |
|---|---|---|
| `create-web-main` | The main column of the page (centered, max-width). | direct child of `body` |
| `create-web-content` | Centered content block with max-width and padding. | inside `main` |
| `create-web-sidebar` | Drawer that slides in from the side. | sibling of `main`, normally `:hidden t` until opened |
| `create-web-sidebar-item` | A clickable row inside a sidebar. | inside `sidebar` |
| `create-web-panel` | Card-style notice/box with a colored class. | inside `main` or `content` |
| `create-web-compositor` | Stacks children at composite positions (top-right, middle, etc.). Used for hero sections. | inside `main` |
| `create-web-code` | Pre-formatted code block. | inside `content` |
| `create-web-row` | A 12-column grid row. | inside `main` |
| `create-web-container` | A column inside `web-row`. | inside `web-row` |
| `create-web-auto-row` | Row that sizes columns to content. | inside `main` |
| `create-web-auto-column` | Column inside `web-auto-row`. | inside `web-auto-row` |

Style sugar: `add-card-look` turns any element into a card (shadow, rounded corners). `(setf (background-color elem) "...")` etc. all still work because everything is still a plain CLOG element underneath.

## The 12-column grid

`create-web-row` plus `create-web-container :column-size ...` gives Bootstrap-style responsive grids. Sizes are `:half`, `:third`, `:quarter`, `:fifth`, `:sixth`, `:eighth`, `:twelfth`, plus numeric `:1` … `:12`. Browsers smaller than the breakpoint collapse columns to full width automatically.

```lisp
(with-clog-create body
    (div (:bind main :class "w3-main")        ; or use create-web-main below
         (h2 (:content "Three columns"))
         (div (:bind row :class "w3-row w3-padding"))))   ; clog-web-row
```

Or imperatively when you have a dynamic number of cards:

```lisp
(let ((row (create-web-row main :padding t)))
  (dolist (card cards)
    (create-web-container row :content (card-html card)
                              :column-size :third
                              :class "w3-border")))
```

Use `:padding t` on the row to space columns apart; turn it off when columns should touch.

## Sidebars

A sidebar is a fixed-position element that you toggle by setting its `display`. Open it from a hamburger button; let users close it from a close item at the top of the drawer.

```lisp
(let ((side (create-web-sidebar body :class "w3-animate-right" :hidden t))
      (main (create-web-main   body)))
  (setf (right side) (unit :px 0))
  (add-card-look side)

  (set-on-click
    (create-web-sidebar-item side :content "Close &times;" :class "w3-teal")
    (lambda (obj) (declare (ignore obj))
      (setf (display side) :none)))
  (create-web-sidebar-item side :content "Home")
  (create-web-sidebar-item side :content "About")

  (let* ((btn (create-button main :content "&#9776;"
                                   :class "w3-button w3-text-white")))
    (set-on-click btn (lambda (obj) (declare (ignore obj))
                        (setf (display side) :block)))))
```

`w3-animate-right` slides the drawer in from the right; `w3-animate-left` for the other side. Both are pure CSS — no JS needed from you.

## Compositors — hero sections and overlays

A compositor stacks children using `position: absolute`, then exposes named slots (`composite-top-right`, `composite-middle`, `composite-bottom-middle`, `composite-position :top N :left N`) to place each child. Perfect for a hero image with overlaid text + button.

```lisp
(let* ((com (create-web-compositor main))
       (img (create-img com :url-src "/img/hero.jpg"))
       (btn (create-button com :content "&#9776;"
                                :class "w3-button w3-text-white"))
       (txt (create-div com :content "CLOG — Beyond Web Frameworks!"
                            :class "w3-center w3-text-white w3-xlarge"))
       (logo (create-img com :url-src "/img/logo.png")))
  (setf (box-width img)  "100%"
        (box-height img) "200")
  (composite-top-right btn)
  (composite-middle    txt)
  (composite-position  logo :top 20 :left 20))
```

The composite-* helpers add the right CSS classes so children align consistently. `composite-position` is the escape hatch for arbitrary offsets.

## Panels

Cheap, eye-catching notice boxes — exactly the right tool for "Saved!" / "Warning:" / "Heads-up" callouts.

```lisp
(create-web-panel main
  :content "<h3>Note:</h3><p>Free shipping over $50.</p>"
  :class "w3-yellow")
```

Stack multiple panels for sectioned content; combine with `add-card-look` for a deeper look. The panel itself is just a styled div — you can `create-*` further children inside it.

## A complete landing-page skeleton

```lisp
(defun on-new-window (body)
  (clog-web-initialize body)
  (setf (title (html-document body)) "Acme")
  (let ((side (create-web-sidebar body :class "w3-animate-left" :hidden t))
        (main (create-web-main   body)))
    ;; sidebar
    (set-on-click (create-web-sidebar-item side :content "Close &times;"
                                                :class "w3-teal")
                  (lambda (o) (declare (ignore o)) (setf (display side) :none)))
    (create-web-sidebar-item side :content "Pricing")
    (create-web-sidebar-item side :content "Docs")
    (create-web-sidebar-item side :content "Sign in")
    ;; hero
    (let* ((com  (create-web-compositor main))
           (hero (create-img com :url-src "/img/hero.jpg"))
           (open (create-button com :content "&#9776;" :class "w3-button"))
           (head (create-div com :content "Painless Lisp web apps"
                                  :class "w3-xxlarge w3-text-white")))
      (setf (box-width hero) "100%" (box-height hero) "260")
      (composite-top-right open)
      (composite-middle head)
      (set-on-click open (lambda (o) (declare (ignore o))
                           (setf (display side) :block))))
    ;; features
    (let ((row (create-web-row main :padding t)))
      (create-web-container row :column-size :third :class "w3-border"
                            :content "<h3>Fast</h3><p>Reload code without restart.</p>")
      (create-web-container row :column-size :third :class "w3-border"
                            :content "<h3>Real</h3><p>Real Lisp on the server.</p>")
      (create-web-container row :column-size :third :class "w3-border"
                            :content "<h3>Free</h3><p>BSD-licensed.</p>"))
    ;; callout
    (create-web-panel main :class "w3-pale-yellow"
                      :content "<b>New:</b> CLOG Builder ships with templates.")))
```

That's a complete responsive landing page in ~25 lines.

## Higher-level helpers

For CMS-style sites — themes, forms with persistence, blog/news indexes — CLOG ships two further layers built on `clog-web`:

- **`clog-web-site`** — themed multi-page sites with menus, footers, login state, theme switching. Tutorial 30 in the upstream repo is the canonical demo.
- **`clog-web-dbi`** / **`clog-auth`** — bind forms directly to SQL tables, manage authority levels. Covered in `db-and-deploy.md`.

Reach for these only when you actually want their opinions; if you want a custom layout, plain `clog-web` is friendlier.

## w3.css cheat sheet (for `:class` keyword)

Most `clog-web` widgets accept a `:class` keyword that's passed straight to the underlying element. The useful w3.css classes:

| Purpose | Classes |
|---|---|
| Colors | `w3-red w3-blue w3-teal w3-green w3-yellow w3-orange w3-pale-red w3-pale-yellow` (pastel variants) |
| Text colors | `w3-text-white w3-text-red w3-text-grey` |
| Layout | `w3-center w3-right-align w3-padding w3-margin w3-border w3-card w3-card-4` |
| Sizes | `w3-tiny w3-small w3-medium w3-large w3-xlarge w3-xxlarge w3-jumbo` |
| Buttons | `w3-button w3-bar-item` |
| Animations | `w3-animate-left w3-animate-right w3-animate-top w3-animate-opacity` |
| Mobile | `w3-mobile` on buttons makes them full-width on small screens |

When you outgrow w3.css, point `clog-web-initialize` at your own stylesheet and use Tailwind or anything else — `:class` is a plain string passthrough.

## Patterns

**One handler builds one page section.** Decomposing into `(build-hero main)`, `(build-features main)`, `(build-footer main)` keeps `on-new-window` readable. Each builder takes its parent container and returns nothing.

**Don't fight responsiveness.** Pixel widths on grid columns defeat the point — let column-size do its job. Reach for explicit widths only inside compositors, where positioning is the whole game.

**Use `with-clog-create` for forms.** Even inside a `web-content` container, forms with several fields are much clearer declaratively. The macro happily nests inside any `clog-web` container.

**Centralize colors.** Don't sprinkle `w3-red` everywhere. Pick a small palette (primary, secondary, success, warning, danger) and bind them to your own `*primary*` etc. parameters at the top of the package. Switching themes later then takes one edit.
