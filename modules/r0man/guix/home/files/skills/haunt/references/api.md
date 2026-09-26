# Haunt API Reference

Exact procedures, modules, and signatures. Read when you need a detail that isn't in SKILL.md.

## Table of contents
- [`(haunt site)` — site record](#haunt-site)
- [`(haunt post)` — post record and helpers](#haunt-post)
- [`(haunt artifact)` — what builders return](#haunt-artifact)
- [Readers](#readers)
- [Builders](#builders)
- [Writing a custom reader](#writing-a-custom-reader)
- [Writing a custom builder](#writing-a-custom-builder)

## `(haunt site)`

```scheme
(site #:title "This Place is Haunted"
      #:domain "example.com"
      #:scheme 'https          ; or 'http
      #:posts-directory "posts"
      #:file-filter default-file-filter
      #:build-directory "site"
      #:default-metadata '()
      #:make-slug post-slug
      #:readers '()
      #:builders '()
      #:publishers '())
```

Accessors: `site-title`, `site-domain`, `site-scheme`, `site-posts-directory`, `site-build-directory`, `site-default-metadata`, `site-make-slug`, `site-readers`, `site-builders`, `site-publishers`, `site-file-filter`, `site-absolute-build-directory`.

Helpers defined in `(haunt site)`:
- `site-post-slug site post` — apply the site's `make-slug` to a post.
- `default-file-filter file-name` — excludes dotfiles and emacs backup files.

## `(haunt post)`

Post record constructor:
```scheme
(make-post file-name metadata sxml)
```

Accessors: `post-file-name`, `post-metadata`, `post-sxml`, `post?`.

Metadata helpers:
- `(post-ref post key)` → value or `#f`
- `(post-ref-all post key)` → list of all values for `key`
- `(post-title post)` — `title` metadata or `#f`
- `(post-date post)` — `date` metadata as an SRFI-19 date, or `%default-date`
- `(post-author post)` — `author` metadata or `#f`
- `(post-tags post)` — list of strings, parsed from comma-separated `tags:`
- `(post-slug post)` — URL slug derived from `slug:` metadata or title
- `(post-slug-v2 post)` — same, but handles dots/numbers more sensibly

Post-list filters and groupers:
- `(posts/reverse-chronological posts)` → posts sorted newest first
- `(posts/group-by-tag posts)` → alist `((tag . posts) ...)`
- `(posts/tag tag)` → a filter procedure that keeps posts tagged `tag`

`%default-date` is the epoch; use it to detect posts missing a date.

## `(haunt artifact)`

Builders return lists of artifacts. Create them with:

```scheme
(serialized-artifact file-name data serializer)
```

- `file-name` is relative to `build-directory`.
- `data` is anything.
- `serializer` is a procedure `(data port) → unspecified` that writes `data` to `port`.

For HTML pages, use the helper from `(haunt html)`:
```scheme
(use-modules (haunt html))
(serialized-artifact "index.html" sxml sxml->html)
```

There's also `(verbatim-artifact file-name string)` for when you already have a string, and `(copy-artifact src dest)` for copying existing files into the output (this is what `static-directory` uses under the hood).

## Readers

A reader record:
```scheme
(use-modules (haunt reader))
(make-reader
  (make-file-extension-matcher "md")   ; predicate on file name
  (lambda (file)                       ; parser
    (call-with-input-file file
      (lambda (port)
        (let ((metadata (read-metadata-headers port)))
          (values metadata (parse-body port)))))))
```

Helpers in `(haunt reader)`:
- `(make-file-extension-matcher ext)` → predicate
- `(read-metadata-headers port)` → alist; reads `key: value` lines until `---`

Built-in readers:

| Reader | Module | Extensions |
|---|---|---|
| `commonmark-reader` | `(haunt reader commonmark)` | `.md` |
| `texinfo-reader` | `(haunt reader texinfo)` | `.texi` |
| `skribe-reader` | `(haunt reader skribe)` | `.skr` |

All three share the same frontmatter format: a block of `key: value` lines, a `---` on its own line, then the body.

## Builders

### `(haunt builder blog)`

```scheme
(blog #:theme ugly-theme         ; default is literally named ugly-theme
      #:prefix #f                ; no default — #f means no prefix
      #:post-prefix #f           ; no default
      #:collections `(("Recent Posts" "index.html" ,posts/reverse-chronological))
      #:posts-per-page #f)       ; #f means no pagination
```

**`blog` has no `#:filter` keyword argument.** To hide posts from a collection, put the filter inside the collection tuple itself — `(collection-title file-name filter-proc)`. The filter is a procedure `(list-of-posts) -> list-of-posts` and is applied before rendering. This is the only way to filter the blog's collections; there's no global filter.

When `posts-per-page` is a number, collections paginate and the theme's `pagination-template` is used.

Also exported from `(haunt builder blog)`:

- **`date->string*`** — takes an SRFI-19 date, returns a string formatted as `"~a ~d ~B ~Y"` (e.g. "Thu 15 January 2025"). Not from `(haunt utils)` — that's a common miswrite.
- **`with-layout`, `render-post`, `render-collection`** — low-level helpers used by `blog` internally; rarely useful to call directly.

Theme constructor (from the same module):
```scheme
(theme #:name "Untitled"
       #:layout           ; (site page-title body-sxml) -> full page sxml
       #:post-template    ; (post) -> post-body sxml
       #:collection-template ; (site title posts url-prefix) -> body sxml
       #:pagination-template) ; (site body previous-file next-file) -> body sxml
```

Predicate: `theme?`.

### `(haunt builder atom)`

```scheme
(atom-feed #:file-name "feed.xml"
           #:subtitle "Recent Posts"
           #:filter posts/reverse-chronological
           #:last-updated (current-date)
           #:max-entries 20
           #:blog-prefix "")

(atom-feeds-by-tag #:prefix "feeds/tags"
                   #:filter posts/reverse-chronological
                   #:last-updated (current-date)
                   #:max-entries 20
                   #:blog-prefix "")
```

### `(haunt builder rss)`

```scheme
(rss-feed #:file-name "rss-feed.xml"
          #:subtitle "Recent Posts"
          #:filter posts/reverse-chronological
          #:publication-date (current-date)
          #:max-entries 20
          #:blog-prefix "")
```

### `(haunt builder assets)`

```scheme
(static-directory source-dir)           ; dest defaults to source-dir
(static-directory source-dir dest-dir)
```

Recursively copies files. `source-dir` is relative to the site source directory; `dest-dir` is relative to `build-directory`.

### `(haunt builder flat-pages)`

```scheme
(flat-pages directory
            #:template layout-proc  ; same shape as theme #:layout
            #:prefix "")
```

Each file in `directory` is parsed with the site's readers and rendered through `template`. Files need at least a `title:` frontmatter field. Nested directory structure is preserved in the output.

### `(haunt builder redirects)`

```scheme
(redirects '(("/old.html" "/new/page.html")
             ("/external.html" "https://example.org/elsewhere")))
```

Emits HTML pages with meta-refresh. `from` must be a local path; `to` can be local or an absolute URL.

## Publishers

Haunt has built-in publishers for rsync and Sourcehut pages. Pass them to `site` via `#:publishers` and invoke with `haunt publish`. See the Haunt manual for details — they're thin wrappers around their respective CLIs.

## Writing a custom reader

```scheme
(use-modules (haunt reader)
             (haunt post)
             (sxml simple))

(define org-reader
  (make-reader
   (make-file-extension-matcher "org")
   (lambda (file)
     (call-with-input-file file
       (lambda (port)
         (let ((metadata (read-metadata-headers port)))
           (values metadata
                   (my-org-to-sxml (read-string port)))))))))
```

Two things to get right:
1. The matcher predicate must return `#t` for file names you want to handle.
2. The parser must return two values: a metadata alist and an SXML tree.

You can reuse `read-metadata-headers` if your files use the standard `key: value\n---\n` header. For richer formats (e.g. org's `#+TITLE:`), parse headers yourself.

## Writing a custom builder

A builder is a procedure `(site posts) → (list artifact ...)`.

```scheme
(use-modules (haunt artifact)
             (haunt html))

(define (json-index)
  (lambda (site posts)
    (list
     (serialized-artifact
      "posts.json"
      (map (lambda (p)
             `((title . ,(post-ref p 'title))
               (date  . ,(date->string* (post-date p)))
               (slug  . ,(site-post-slug site p))))
           (posts/reverse-chronological posts))
      write-json))))  ; your JSON writer
```

The outer wrapper (`(define (json-index) (lambda ...))`) is the convention so builders look like the built-ins when listed in `#:builders`. If your builder takes no config you can just `(define json-index (lambda (site posts) ...))` and add it as `json-index` rather than `(json-index)`.

Anything you can compute from `site` and `posts` you can emit as an artifact. A few ideas:
- Sitemap XML
- Search index (lunr, tinysearch)
- Per-author pages
- A plain-text archive
