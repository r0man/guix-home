# Haunt site examples

Copy-pasteable starting points for common site shapes. Each example is complete and should build with `haunt build` once you have the matching `posts/` directory and dependencies installed.

## 1. Minimal Markdown blog

Just posts, an index, and an Atom feed. Uses Haunt's default theme.

```scheme
;; haunt.scm
(use-modules (haunt site)
             (haunt builder blog)
             (haunt builder atom)
             (haunt reader commonmark))

(site #:title "Ada's Notebook"
      #:domain "https://ada.example.com"
      #:default-metadata
      '((author . "Ada Lovelace")
        (email  . "ada@example.com"))
      #:readers (list commonmark-reader)
      #:builders (list (blog)
                       (atom-feed)
                       (atom-feeds-by-tag)))
```

Directory layout:
```
./haunt.scm
./posts/
  hello.md
  next-post.md
```

Post frontmatter:
```
title: Hello, world!
date: 2025-01-15 10:00
tags: intro

# Hi
Body goes here.
```

(Note: the `---` sentinel separator goes between the metadata block and the body. The first line after it is the start of the body.)

## 2. Blog + flat pages + static assets + custom theme

A more realistic site. About page, CSS, images, and a custom theme.

```scheme
;; haunt.scm
(use-modules (haunt site)
             (haunt post)
             (haunt html)
             (haunt builder blog)
             (haunt builder atom)
             (haunt builder assets)
             (haunt builder flat-pages)
             (haunt reader commonmark)
             (srfi srfi-19))

;;; --- Theme ---

(define (format-date date)
  (if (equal? date %default-date)
      ""
      (date->string date "~B ~e, ~Y")))

(define (base-layout site title body)
  `((doctype "html")
    (html (@ (lang "en"))
     (head
      (meta (@ (charset "utf-8")))
      (meta (@ (name "viewport")
               (content "width=device-width, initial-scale=1")))
      (title ,(string-append title " — " (site-title site)))
      (link (@ (rel "stylesheet") (href "/css/style.css"))))
     (body
      (header (@ (class "site-header"))
       (h1 (a (@ (href "/")) ,(site-title site)))
       (nav (ul (li (a (@ (href "/")) "Home"))
                (li (a (@ (href "/about.html")) "About"))
                (li (a (@ (href "/feed.xml")) "RSS")))))
      (main ,body)
      (footer "© 2025 " ,(site-title site))))))

(define (post-template post)
  `((article
     (header
      (h1 ,(post-ref post 'title))
      (time ,(format-date (post-date post))))
     ,(post-sxml post))))

(define (collection-template site title posts prefix)
  `((h2 ,title)
    (ul (@ (class "posts"))
        ,@(map (lambda (p)
                 `(li (a (@ (href ,(string-append "/" prefix "/"
                                                  (site-post-slug site p)
                                                  ".html")))
                         ,(post-ref p 'title))
                      " — "
                      (time ,(format-date (post-date p)))))
               posts))))

(define notebook-theme
  (theme #:name "notebook"
         #:layout base-layout
         #:post-template post-template
         #:collection-template collection-template))

;;; --- Site ---

(site #:title "Ada's Notebook"
      #:domain "https://ada.example.com"
      #:default-metadata
      '((author . "Ada Lovelace")
        (email  . "ada@example.com"))
      #:readers (list commonmark-reader)
      #:builders
      (list (blog #:theme notebook-theme)
            (atom-feed)
            (atom-feeds-by-tag)
            (flat-pages "pages" #:template base-layout)
            (static-directory "css")
            (static-directory "images")))
```

Layout:
```
./haunt.scm
./posts/*.md
./pages/about.md        ; needs "title:" in frontmatter
./css/style.css
./images/avatar.png
```

## 3. Mixed Markdown and Texinfo

Same site, but some posts are Texinfo. Just add the reader.

```scheme
(use-modules (haunt site)
             (haunt builder blog)
             (haunt builder atom)
             (haunt reader commonmark)
             (haunt reader texinfo))

(site #:title "Mixed"
      #:domain "https://example.com"
      #:readers (list commonmark-reader texinfo-reader)
      #:builders (list (blog) (atom-feed)))
```

Files ending in `.md` go through `commonmark-reader`; files ending in `.texi` go through `texinfo-reader`. Both use the same frontmatter format.

## 4. Filtering drafts

Custom collection filter keeps `draft: true` posts out of the index and feeds.

```scheme
(use-modules (haunt site)
             (haunt post)
             (haunt builder blog)
             (haunt builder atom)
             (haunt reader commonmark))

(define (published posts)
  (filter (lambda (p) (not (post-ref p 'draft))) posts))

(define (recent posts)
  (posts/reverse-chronological (published posts)))

(site #:title "Draft-aware"
      #:domain "https://example.com"
      #:readers (list commonmark-reader)
      #:builders
      (list (blog #:collections `(("Recent" "index.html" ,recent)))
            (atom-feed #:filter recent)))
```

Adding `draft: true` to a post's frontmatter hides it from the index and feed. `haunt build` still writes an individual page for it, so you can preview it directly.

## 5. Custom builder: sitemap.xml

Demonstrates writing your own builder that returns a single artifact.

```scheme
(use-modules (haunt site)
             (haunt post)
             (haunt artifact)
             (haunt builder blog)
             (haunt reader commonmark)
             (sxml simple))

(define (sitemap)
  (lambda (site posts)
    (define (url path)
      `(url (loc ,(string-append (site-domain site) path))))
    (define body
      `(urlset (@ (xmlns "http://www.sitemaps.org/schemas/sitemap/0.9"))
               ,(url "/")
               ,@(map (lambda (p)
                        (url (string-append "/posts/"
                                            (site-post-slug site p)
                                            ".html")))
                      posts)))
    (list
     (serialized-artifact
      "sitemap.xml"
      body
      (lambda (sxml port) (sxml->xml sxml port))))))

(site #:title "With sitemap"
      #:domain "https://example.com"
      #:readers (list commonmark-reader)
      #:builders (list (blog) (sitemap)))
```

The pattern: a builder is `(site posts) → (list artifact ...)`. `serialized-artifact` takes a file name, a data value, and a writer procedure that sends the data to a port.

## 6. Running under Guix

If the user's environment is Guix-based (like this repo), the most frictionless way to build a Haunt site is a one-off shell:

```bash
cd my-haunt-site/
guix shell haunt guile-commonmark -- haunt build
guix shell haunt guile-commonmark -- haunt serve --watch
```

Or to add Haunt permanently to the home profile, append it to the appropriate package list in `modules/r0man/guix/home/packages.scm` (or to a `haunt.scm` home module) and run `guix home reconfigure`.

For Texinfo posts, no extra reader library is needed — it ships with Guile. For Skribe, add `guile-reader`.

## 7. Directory tree cheat sheet

A typical site ends up looking like:

```
my-site/
├── haunt.scm          ; the site program
├── posts/             ; blog posts, one file each
│   ├── 2025-01-15-hello.md
│   └── 2025-02-01-follow-up.md
├── pages/             ; flat-pages input (optional)
│   ├── about.md
│   └── colophon.md
├── css/               ; static assets (optional)
│   └── style.css
├── images/            ; more assets (optional)
└── site/              ; generated output (gitignore this)
```

`build-directory` defaults to `site` — add it to `.gitignore`.
