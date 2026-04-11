---
name: haunt
description: Expert knowledge for building static websites with Haunt, the functional Scheme-based static site generator by David Thompson. Use this skill whenever the user mentions Haunt, wants to scaffold a new static site in Guile/Scheme, asks about haunt.scm configuration, writes posts in CommonMark or Texinfo for Haunt, needs builders (blog, atom-feed, flat-pages, static-directory), writes or debugs Haunt themes using SXML, or integrates Haunt with GNU Guix. Trigger even when the user just says "static site" or "blog" in a Scheme/Guile context.
---

# Haunt — Functional Static Site Generator

Haunt is a static site generator written in Guile Scheme. Unlike Jekyll or Hugo, a Haunt site *is* a Scheme program: you compose *readers* (parse source files into posts) and *builders* (turn posts and metadata into output artifacts) into a `site` record, and call `haunt build` to render everything.

The whole generator is small and the entire pipeline is functional. This means you rarely fight a framework — when something's missing, you write a small procedure and pass it to `site`.

## The mental model

```
source files ─► readers ─► posts ─► builders ─► artifacts ─► build-directory
                                     ▲
                           site record (metadata, theme, config)
```

- **Readers** turn files in `posts/` into `<post>` records (metadata alist + SXML body). One reader per file extension.
- **Builders** are procedures `site → posts → (list artifact ...)`. Haunt ships `blog`, `atom-feed`, `atom-feeds-by-tag`, `rss-feed`, `static-directory`, `flat-pages`, `redirects`.
- **Themes** are records of SXML-producing procedures used by the `blog` builder. Pages are just SXML — no template language.
- **Artifacts** are `(name . writer)` pairs; Haunt writes them under `build-directory`.

Internalising this makes the rest fall into place — if a user wants some custom output, the answer is almost always "write a builder" or "extend the theme".

## Minimum viable site

A working Haunt site needs three things: a `haunt.scm`, a `posts/` directory, and (usually) at least one builder.

```scheme
;; haunt.scm
(use-modules (haunt site)
             (haunt builder blog)
             (haunt builder atom)
             (haunt reader commonmark))

(site #:title "My Blog"
      #:domain "https://example.com"
      #:default-metadata
      '((author . "Ada Lovelace")
        (email  . "ada@example.com"))
      #:readers (list commonmark-reader)
      #:builders (list (blog)
                       (atom-feed)
                       (atom-feeds-by-tag)))
```

```markdown
<!-- posts/hello.md -->
title: Hello, world!
date: 2025-01-15 10:00
tags: intro, haunt
---

# Hello!

This is my first Haunt post.
```

```
$ haunt build         # renders everything into ./site/
$ haunt serve --watch # http://localhost:8080, rebuilds on save
```

Frontmatter syntax is the same for every reader Haunt ships: `key: value` lines, then a literal `---` delimiter on its own line, then the body. **Not YAML** — Haunt parses a simpler line-based format. Don't wrap values in quotes, don't indent, don't use nested keys.

## The `site` record

Every Haunt site starts with a call to `site`. All keyword arguments are optional:

| Keyword | Default | Purpose |
|---|---|---|
| `#:title` | `"This Place is Haunted"` | Site name, used in feeds and themes |
| `#:domain` | `"example.com"` | Canonical URL, used to build absolute links in feeds |
| `#:scheme` | `'https` | `'https` or `'http` — used with domain for absolute URLs |
| `#:posts-directory` | `"posts"` | Where readers look for source files |
| `#:build-directory` | `"site"` | Where builders write output |
| `#:default-metadata` | `'()` | Alist merged into every post's metadata |
| `#:make-slug` | `post-slug` | How titles become URL slugs |
| `#:file-filter` | `default-file-filter` | Predicate to exclude files (e.g. dotfiles) |
| `#:readers` | `'()` | List of reader records |
| `#:builders` | `'()` | List of builder procedures |
| `#:publishers` | `'()` | List of publisher records (rsync, sourcehut) |

## Readers

A reader declares a file extension matcher and a procedure that returns `(values metadata sxml)`. Built-in readers:

- **`commonmark-reader`** from `(haunt reader commonmark)` — CommonMark markdown. Requires `guile-commonmark`.
- **`texinfo-reader`** from `(haunt reader texinfo)` — GNU Texinfo. Ships with Guile.
- **`skribe-reader`** from `(haunt reader skribe)` — Scheme-flavoured markup. Requires `guile-reader`.

All of them use the same `key: value\n---\n` frontmatter convention. Pick readers based on what the user wants to write in; you can pass multiple readers to one `site` and mix formats.

Writing a custom reader is a small amount of code — see `references/api.md` for the pattern and a worked example.

## Builders

Haunt ships the following builders. Each is a *procedure that returns a builder* — you call it with keyword args and put the result in `#:builders`.

### `blog` — from `(haunt builder blog)`

Generates per-post pages and collection index pages (e.g. "Recent Posts").

```scheme
(blog #:theme my-theme
      #:prefix ""                      ; URL prefix for the whole blog
      #:post-prefix "posts"            ; URL prefix for individual posts
      #:collections `(("Recent Posts" "index.html" ,posts/reverse-chronological)
                      ("Guile"        "guile.html" ,(posts/tag "guile")))
      #:posts-per-page 10)             ; paginate if set
```

Collections are `(title filename filter-proc)` — the filter takes a list of posts and returns the posts to include. `posts/reverse-chronological` and `posts/group-by-tag` come from `(haunt post)`.

### `atom-feed`, `atom-feeds-by-tag` — from `(haunt builder atom)`

```scheme
(atom-feed #:file-name "feed.xml"
           #:subtitle "Recent Posts"
           #:filter posts/reverse-chronological
           #:max-entries 20
           #:blog-prefix "")

(atom-feeds-by-tag #:prefix "feeds/tags"   ; one feed per tag
                   #:max-entries 20
                   #:blog-prefix "")
```

### `rss-feed` — from `(haunt builder rss)`

Same shape as `atom-feed`, default file name `"rss-feed.xml"`.

### `static-directory` — from `(haunt builder assets)`

```scheme
(static-directory "images")          ; copies ./images to site/images
(static-directory "css" "assets/css") ; optional destination
```

### `flat-pages` — from `(haunt builder flat-pages)`

For informational pages (About, Contact) that aren't posts:

```scheme
(flat-pages "pages"
            #:template my-layout   ; REQUIRED: (site title body-sxml) -> full page SXML
            #:prefix "")           ; optional, defaults to "/"
```

**`#:template` is effectively required.** It has no default — if you omit it, Haunt will try to call `#f` as a procedure at build time and crash. When `blog` is also in the builder list you can reuse its theme's layout: `(flat-pages "pages" #:template (theme-layout my-theme))`. Otherwise define a tiny layout procedure and point both `flat-pages` and the blog theme at it — that's the shape used in `references/examples.md`.

Each file in `pages/` needs at minimum a `title:` metadata field. Files are parsed with the site's readers, so you can write them as markdown.

### `redirects` — from `(haunt builder redirects)`

```scheme
(redirects '(("/old.html" "/new/page.html")
             ("/guile.html" "https://www.gnu.org/software/guile/")))
```

Emits tiny HTML pages with meta-refresh. `from` must be a local file name; `to` can be local or an external URL.

## Themes

A theme is what makes the `blog` builder produce *something that looks like a website*. It's a record of SXML-producing procedures:

```scheme
(use-modules (haunt site)        ; site-title, site-post-slug
             (haunt post)        ; post-ref, post-date, post-sxml
             (haunt builder blog) ; theme constructor AND date->string*
             (srfi srfi-19))     ; date->string with format strings

(define my-theme
  (theme
   #:name "My Theme"
   #:layout
   (lambda (site title body)
     ;; body is the inner SXML; wrap it in <html>
     `((doctype "html")
       (html
        (head (title ,(string-append title " — " (site-title site)))
              (meta (@ (charset "utf-8")))
              (link (@ (rel "stylesheet") (href "/css/style.css"))))
        (body
         (header (h1 (a (@ (href "/")) ,(site-title site))))
         (main ,body)
         (footer "© " ,(site-title site))))))
   #:post-template
   (lambda (post)
     `((article
        (h1 ,(post-ref post 'title))
        (div (@ (class "date")) ,(date->string* (post-date post)))
        ,(post-sxml post))))
   #:collection-template
   (lambda (site title posts prefix)
     `((h2 ,title)
       (ul
        ,@(map (lambda (p)
                 `(li (a (@ (href ,(string-append "/" prefix "/"
                                                  (site-post-slug site p)
                                                  ".html")))
                         ,(post-ref p 'title))
                      " — "
                      ,(date->string* (post-date p))))
               posts))))))
```

SXML is lists-as-HTML. `(@ (attr "val"))` puts attributes on an element. `,(...)` unquotes into the expression. Haunt renders SXML to HTML5 internally.

### Where things actually live (critical)

Haunt's module layout is unintuitive enough that guessing wrong is easy. A few exact facts:

- **`theme` (the constructor) is exported from `(haunt builder blog)`**, not from any standalone `(haunt theme)` module — **that module does not exist**. Importing it will fail with "no code for module". If you need `theme`, you already have it via `(haunt builder blog)`.
- **`date->string*` is also exported from `(haunt builder blog)`**, not from `(haunt utils)`. It's a simple helper that calls SRFI-19's `date->string` with the format `"~a ~d ~B ~Y"`. If you want a different format, just use `(date->string date "~Y-~m-~d")` from `(srfi srfi-19)` directly and skip the helper.
- **`site-title`, `site-domain`, `site-post-slug`** come from `(haunt site)`.
- **`post-ref`, `post-date`, `post-sxml`, `post-tags`, `post-title`, `posts/reverse-chronological`, `posts/group-by-tag`, `%default-date`** come from `(haunt post)`.
- **`(haunt page)` and `(haunt asset)` are DEPRECATED legacy modules.** Don't import them. The current equivalents are `serialized-artifact` and `verbatim-artifact` from `(haunt artifact)`, and builders return artifact lists directly — you rarely need `(haunt artifact)` either unless you're writing a custom builder.

For anything more than a single layout you'll want to read `references/themes.md`.

## Post metadata

Inside a post body, call `post-ref`, `post-title`, `post-date`, `post-tags`, `post-author` to pull metadata out. `post-sxml` returns the body as SXML. `post-slug` (and `post-slug-v2`) turn a post into a URL-safe slug from its title (or a custom `slug:` metadata field).

Useful filters from `(haunt post)`:
- `posts/reverse-chronological` — newest first
- `posts/group-by-tag` — alist of tag → posts
- `(posts/tag tag)` — filter procedure that keeps posts tagged `tag`

## CLI

- **`haunt build`** — render everything into `build-directory`. Fails if any builder throws.
- **`haunt serve`** — static file server on `http://localhost:8080`. Add `--watch` to rebuild on change, `--port N` to change port, `--host 0.0.0.0` to bind externally.
- **`haunt publish`** — invokes a publisher to ship the built site elsewhere.

Haunt looks for `haunt.scm` in the current directory unless you pass `--config`.

## Running Haunt

Haunt needs Guile and the reader libraries for whatever formats you're using.

**On GNU Guix** (the native and most reliable option):

```bash
# Temporary shell — no install
guix shell haunt guile-commonmark -- haunt build

# Permanent — add to home profile
# (in your manifest) (specifications->manifest '("haunt" "guile-commonmark"))
```

If the user is editing a Haunt site inside this repo's guix-home, the natural move is to add `haunt` and its reader deps to `packages-base` or the relevant module in `home/packages.scm`.

**From source:** clone `https://git.dthompson.us/haunt.git`, `./bootstrap && ./configure && make`. Needs Guile ≥ 2.0.

## Common tasks

**Add a new post.** Create `posts/<slug>.md` (or `.texi`). Add frontmatter with at least `title:` and `date:`. `haunt build` picks it up automatically — no index to update.

**Add a new page (not a blog post).** Put it in a `pages/` directory and add `(flat-pages "pages" #:template my-layout)` to `#:builders`. The `#:template` argument is required — without it, the build crashes. If the site already has a blog theme, reuse its layout with `(flat-pages "pages" #:template (theme-layout my-theme))` so all pages share the same chrome. For a one-off file like `404.html`, write a tiny custom builder that returns a single artifact.

**Add an RSS feed alongside Atom.** Add `(rss-feed)` to `#:builders`, import `(haunt builder rss)`.

**Serve static CSS/images.** Add `(static-directory "css")` and `(static-directory "images")` to `#:builders`.

**Redirect an old URL.** Add `(redirects '(("/old" "/new")))` to `#:builders`.

**Add tag pages.** Write a builder that iterates over `(posts/group-by-tag posts)` and returns one artifact per tag. See `references/examples.md`.

**Debug a build failure.** Run `haunt build` and read the Guile backtrace. Common causes: missing reader for a file extension, a typo in frontmatter (remember, `key: value`, then `---`), a builder that throws because a post is missing expected metadata.

## Reference files

These are separate so this file stays short. Read them as needed.

- **`references/api.md`** — Full API reference: every procedure in `(haunt site)`, `(haunt post)`, `(haunt artifact)`, every reader and builder with all keyword arguments, plus how to write a custom reader or builder. Read this when you need an exact signature or you're going beyond what's shown above.
- **`references/themes.md`** — Writing themes in depth: SXML patterns, `pagination-template`, sharing layout code, CSS integration, emitting raw HTML, loading partials from files.
- **`references/examples.md`** — Complete `haunt.scm` files for common shapes: minimal blog, blog + flat pages, multi-reader site, site with a custom builder emitting JSON. Look here first when scaffolding — it's usually faster to adapt an example than to write from scratch.

## Design guidance

When a user asks you to build something in Haunt, think in terms of the pipeline (reader → post → builder → artifact) and resist the urge to reach for escape hatches. A few principles:

- **Prefer builders over post-processing.** If you'd normally run a script after `haunt build`, write it as a builder instead — you get the site record and all posts for free.
- **Metadata is just an alist.** You can put arbitrary keys in post frontmatter and read them with `post-ref`. No schema. This is great for things like `draft: true`, `weight: 3`, `layout: wide`.
- **Themes aren't monolithic.** Break layout procedures into helpers (e.g. `(nav site)`, `(post-footer post)`). Themes are Scheme; all the usual abstractions apply.
- **The source is readable.** When in doubt about a builder's exact behaviour, the Haunt source is a few hundred lines per module. Point the user at it — often faster than guessing from docs.
