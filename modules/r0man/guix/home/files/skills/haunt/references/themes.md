# Writing Haunt themes

A theme is a record of four SXML-producing procedures. Everything else follows from "SXML is just lists shaped like HTML".

## SXML in 60 seconds

SXML represents HTML as Scheme lists.

| HTML | SXML |
|---|---|
| `<p>hi</p>` | `'(p "hi")` |
| `<p class="note">hi</p>` | `'(p (@ (class "note")) "hi")` |
| `<a href="/x">link</a>` | `'(a (@ (href "/x")) "link")` |
| `<!DOCTYPE html>` | `'(doctype "html")` |
| `<br>` | `'(br)` |
| Raw string `"hi &amp; hello"` | `'(raw "hi &amp; hello")` |

`,expr` inside a quasiquoted list splices in a value. `,@list` splices a list of elements. So:

```scheme
`(ul ,@(map (lambda (x) `(li ,x)) items))
```

produces `<ul><li>...</li><li>...</li></ul>`.

Haunt renders SXML via `(haunt html)`'s `sxml->html`. Attribute values are auto-escaped; to emit raw HTML (e.g. a widget embed) wrap a string with `(raw ...)`.

## Anatomy of a theme

```scheme
(use-modules (haunt site)          ; site-title, site-post-slug
             (haunt post)          ; post-ref, post-date, etc.
             (haunt builder blog)) ; theme constructor AND date->string*

(define my-theme
  (theme
   #:name "my-theme"
   #:layout           my-layout
   #:post-template    my-post-template
   #:collection-template my-collection-template
   #:pagination-template my-pagination-template))
```

Each slot has a known signature; the blog builder calls them at specific points.

### `#:layout`  —  the HTML shell

Signature: `(lambda (site title body) -> sxml)`

This is the outermost template. It receives the site record, a page title string, and a body SXML tree. It must return a complete page starting with `(doctype "html")`. Everything — posts, collections, paginated pages — goes through `layout`, so all shared `<head>` content lives here.

```scheme
(define (my-layout site title body)
  `((doctype "html")
    (html (@ (lang "en"))
     (head
      (meta (@ (charset "utf-8")))
      (meta (@ (name "viewport")
               (content "width=device-width, initial-scale=1")))
      (title ,(string-append title " — " (site-title site)))
      (link (@ (rel "stylesheet") (href "/css/style.css")))
      (link (@ (rel "alternate")
               (type "application/atom+xml")
               (href "/feed.xml"))))
     (body
      ,(site-header site)
      (main (@ (class "content")) ,body)
      ,(site-footer site)))))
```

Factor out helpers like `site-header`, `site-footer`, `nav` — they're just Scheme.

### `#:post-template` — a single post's inner HTML

Signature: `(lambda (post) -> sxml)`

Receives a post, returns the *inner* SXML that will be passed to `layout` as `body`. Don't emit `<html>` or `<head>` here — `layout` handles that.

```scheme
(define (my-post-template post)
  `((article (@ (class "post"))
     (header
      (h1 ,(post-ref post 'title))
      (time (@ (datetime ,(date->string* (post-date post) "~1")))
            ,(date->string* (post-date post) "~B ~e, ~Y")))
     ,(post-sxml post)
     ,(post-footer post))))
```

### `#:collection-template` — index pages

Signature: `(lambda (site title posts prefix) -> sxml)`

Called for each collection declared in `(blog #:collections ...)`. `posts` is already filtered by the collection's filter. `prefix` is the post URL prefix so you can construct correct hrefs.

```scheme
(define (my-collection-template site title posts prefix)
  `((h2 ,title)
    (ul (@ (class "post-list"))
        ,@(map (lambda (p)
                 `(li
                   (a (@ (href ,(string-append "/" prefix "/"
                                               (site-post-slug site p)
                                               ".html")))
                      ,(post-ref p 'title))
                   " "
                   (time ,(date->string* (post-date p) "~Y-~m-~d"))))
               posts))))
```

Note: `site-post-slug` is the right way to get the URL slug — it uses whatever `#:make-slug` the site was configured with, so if the user swaps in `post-slug-v2` your theme still works.

### `#:pagination-template` — optional

Signature: `(lambda (site body previous-file next-file) -> sxml)`

Only called when `(blog #:posts-per-page N)` is set. It wraps a paginated collection page body with prev/next links. `previous-file` and `next-file` are strings (or `#f`) naming the adjacent page files.

```scheme
(define (my-pagination-template site body prev next)
  `(,body
    (nav (@ (class "pagination"))
     ,(if prev `(a (@ (href ,prev)) "← Newer") "")
     ,(if next `(a (@ (href ,next)) "Older →") ""))))
```

## Patterns

### Shared partials

Themes are Scheme. Define helpers:

```scheme
(define (nav)
  `(nav (ul (li (a (@ (href "/")) "Home"))
            (li (a (@ (href "/about.html")) "About"))
            (li (a (@ (href "/feed.xml")) "RSS")))))
```

And call them inside `layout`, `post-template`, etc.

### Loading CSS from a file

Treat CSS like any other static asset: put it under `css/` and add `(static-directory "css")` to the site's builders. Reference it with an absolute path from the layout.

For tiny sites you can also inline CSS by reading a file at load time:

```scheme
(define css (call-with-input-file "style.css" get-string-all))

(define (my-layout site title body)
  `((doctype "html")
    (html (head (style ,css) ...) ...)))
```

This makes the site self-contained but rebuilds less incrementally.

### Date formatting

`post-date` returns an SRFI-19 `date` record. Format it with `date->string`:

```scheme
(use-modules (srfi srfi-19))

(date->string (post-date post) "~B ~e, ~Y") ; "January 15, 2025"
```

Haunt exports a helper `date->string*` from **`(haunt builder blog)`** (not from `(haunt utils)` — that's an easy mistake). It's a trivial wrapper: `(date->string date "~a ~d ~B ~Y")`. Use it when you want Haunt's default format, or skip it and call SRFI-19 `date->string` directly for full control.

If a post lacks a `date:` field, `post-date` returns `%default-date` (the Unix epoch). `date->string*` doesn't do anything special with that — it'll happily render "Thu 01 January 1970". If you want dateless posts to show nothing, test explicitly: `,(if (equal? (post-date post) %default-date) "" (date->string (post-date post) "~B ~e, ~Y"))`.

### Reading `draft:` metadata

Any custom frontmatter is available via `post-ref`. Filter drafts out with a collection filter:

```scheme
(define (not-drafts posts)
  (filter (lambda (p) (not (post-ref p 'draft))) posts))

(blog #:collections
      `(("Recent Posts" "index.html"
         ,(lambda (posts)
            (posts/reverse-chronological (not-drafts posts))))))
```

### Multiple themes on one site

You can run the `blog` builder more than once with different themes and different prefixes — for example, a full site plus a "print" view. Just add two `(blog ...)` calls to `#:builders` with distinct `#:prefix` values.

## Things to avoid

- **Escaping SXML manually.** Don't concatenate strings into HTML; use SXML and let `sxml->html` escape. If you must emit raw HTML, use `(raw "...")`.
- **Assuming every post has every field.** Use `post-ref` + `or` with a default:
  `,(or (post-ref post 'summary) "")`
- **Hardcoding the domain.** Use `(site-domain site)` if you need absolute URLs; this makes the theme portable.
- **Doing I/O in template procedures.** Read files at the top level so the work happens once per build, not once per page.
