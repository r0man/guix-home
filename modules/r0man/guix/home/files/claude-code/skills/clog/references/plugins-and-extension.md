# Extending CLOG — plugins and custom element classes

CLOG is unusually friendly to extension because the framework is "just CLOS". Every element is an object whose class inherits from `clog-element` (which inherits from `clog-obj`). To build a reusable component you subclass an existing CLOG class, expose a `create-XXX` constructor, and ship the package — that's it. No build step, no JS bundler, no registration with a global widget registry.

Use this technique when you find yourself copy-pasting the same `let* (...)` tree across multiple pages, or when you want to name a domain concept ("rated-input", "currency-picker", "ticker-row") rather than reassembling primitive widgets every time.

## The pattern

Three pieces:

1. **A class** that subclasses an existing CLOG class — usually the closest HTML element (`clog-div`, `clog-unordered-list`, `clog-form-element`, `clog-button`).
2. **A constructor function** `create-XXX` that calls the parent's constructor, then *upgrades* the returned object to your new class via `change-class`, then initializes the component-specific slots and wires events.
3. **Optional accessors / methods** for component-specific behaviour, exposed as generic functions so users can specialize them further.

Skeleton:

```lisp
(defpackage #:clog-rated-input
  (:use #:cl #:clog)
  (:export clog-rated-input
           create-rated-input
           rating))

(in-package :clog-rated-input)

(defclass clog-rated-input (clog-element)
  ((input  :accessor rated-input-input)
   (stars  :accessor rated-input-stars))
  (:documentation "Text input with a 5-star rating widget beside it."))

(defgeneric rating (rated-input)
  (:documentation "Current rating as an integer 0–5."))

(defgeneric create-rated-input (parent &key initial label class html-id auto-place)
  (:documentation "Create a rated-input."))

(defmethod create-rated-input ((parent clog-obj)
                               &key (initial 0)
                                    (label "")
                                    (class nil)
                                    (html-id nil)
                                    (auto-place t))
  (let* ((wrap   (create-div parent :class class :html-id html-id
                                    :auto-place auto-place))
         (lbl    (create-label wrap :content label))
         (input  (create-form-element wrap :text))
         (stars  (create-span wrap)))
    (declare (ignore lbl))
    (change-class wrap 'clog-rated-input)
    (setf (rated-input-input wrap) input
          (rated-input-stars wrap) stars)
    (set-rating wrap initial)
    (dotimes (i 5)
      (let ((idx (1+ i)))
        (set-on-click
          (create-span stars :content (if (<= idx initial) "★" "☆"))
          (lambda (obj) (declare (ignore obj))
            (set-rating wrap idx)))))
    wrap))

(defmethod rating ((c clog-rated-input))
  (parse-integer (or (attribute c "data-rating") "0")))

(defun set-rating (c new)
  (setf (attribute c "data-rating") (princ-to-string new))
  (setf (inner-html (rated-input-stars c))
        (with-output-to-string (s)
          (dotimes (i 5)
            (write-string (if (< i new) "★" "☆") s)))))
```

Used like any built-in:

```lisp
(defpackage #:demo (:use #:cl #:clog #:clog-rated-input))
(in-package :demo)

(defun on-new-window (body)
  (let ((r (create-rated-input body :initial 3 :label "How was it?")))
    (set-on-click (create-button body :content "Show")
                  (lambda (o) (declare (ignore o))
                    (alert (window body) (princ-to-string (rating r)))))))
```

## Key idioms

**`change-class` is the trick.** CLOG's `create-*` functions instantiate the *parent* class first. You let them do that (so the underlying DOM node is created, attached to the parent, gets an `html-id`, …), then `change-class` retypes the existing object to your subclass *in place*. The DOM and Lisp-side state are preserved; only the class pointer changes.

**Initialize slots after `change-class`.** Trying to `:initarg` slots through the parent constructor doesn't work because CLOG's constructors don't forward unknown keys. Set slots after the retype.

**Compose, don't reach in.** Your component is opaque from the outside — callers see one `clog-rated-input`. Internally, you keep the sub-widgets in slots. Don't ask callers to know that "stars" is a `clog-span`; expose `rating`, `set-rating`, `set-on-change`, etc. as part of the contract.

**Specialize generics, don't shadow names.** If the parent class already has `set-on-change`, write `(defmethod set-on-change ((c clog-rated-input) handler) ...)` to call up via `call-next-method` when appropriate. Don't `defun set-on-change` and shadow the symbol.

**Auto-place vs not.** Pass `:auto-place` through to the parent constructor so callers can defer placement (`(setf (attach …))` after configuration). Default to `t` because that's what built-in widgets do.

## A simpler example: the drop-list

When you don't need state, the whole pattern collapses. Tutorial 21 in the upstream repo defines a collapsible list by subclassing `clog-unordered-list`:

```lisp
(defpackage #:clog-drop-list
  (:use #:cl #:clog)
  (:export clog-drop-list create-drop-list drop-root))

(in-package :clog-drop-list)

(defclass clog-drop-list (clog-unordered-list)
  ((drop-root :accessor drop-root)))

(defmethod create-drop-list ((parent clog-obj) &key (content "")
                                                    (class nil)
                                                    (html-id nil)
                                                    (auto-place t))
  (let* ((root   (create-unordered-list parent :class class :html-id html-id
                                                :auto-place auto-place))
         (header (create-list-item root :content content)))
    (change-class root 'clog-drop-list)
    (setf (drop-root root) (create-unordered-list header))
    (set-on-mouse-down header
                       (lambda (obj data)
                         (declare (ignore obj data))
                         (setf (hiddenp (drop-root root))
                               (not (hiddenp (drop-root root)))))
                       :cancel-event t)
    root))
```

`:cancel-event t` on `set-on-mouse-down` prevents the click from bubbling to parents — important for nested lists where each header would otherwise toggle its ancestors.

## When to write a JavaScript plugin instead

CLOG can also wrap an existing JS component (Bootstrap modals, Chart.js charts, CodeMirror editors, …). Pattern:

1. Inject the library's `<script>` and `<link>` via `(setf (style (html-document body)) "...")` or `(create-child (head-element …))`.
2. `(js-execute obj "new ChartJS(document.getElementById('...'), {...})")` from your constructor.
3. Mirror the component's API as Lisp methods that synthesize JS calls.

Upstream tutorials 19 and 20 walk this end-to-end. The Lisp-only pattern (this file) should be your first reach; JS-wrapper is the right call when you genuinely want a giant existing component (a rich text editor, a charting library), not when you'd be reimplementing it.

## Packaging

A CLOG plugin is an ordinary ASDF system — one `.asd`, one `.lisp` per component, a README. Publish via QuickLisp's "quicklisp-projects" PR process or Ultralisp. The community ships dozens already (see the `clog-*` namespace in QuickLisp).

If your plugin needs server-side assets (a JS library, CSS), ship them under `static-files/` in your system's source tree and tell users to either copy that directory under their `:static-root` or set up an Apache `Alias` to it. There is no global asset registration mechanism — keep it simple.
