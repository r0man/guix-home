# Database, auth, server, deployment

This page covers the parts of CLOG that touch the outside world: SQL persistence (`clog-data`, `clog-dbi`), authentication and authority (`clog-auth`), server keyword arguments to `initialize`, threading discipline for long-running workloads, and how to deploy a CLOG app behind a reverse proxy in production.

## clog-data — Lisp-side binding to SQL rows

`clog-data` ships table-row helpers that map CLOG form inputs to SQL columns. The model is *single-row binding*: you point a form (or a set of free-floating inputs) at a row identified by primary key; field values shuttle through automatically.

Two layers exist:

- **`clog-data`** itself — plain helpers usable with any SQL library. You wire CLOG inputs to column names and decide when to load/save.
- **`clog-dbi`** — opinionated bridge to CL-DBI; gives you a CLOS object per row, with `update-record`, `delete-record`, `query-record` style operations.

For most apps the dbi layer is the right starting point. Use `clog-data` directly only when you already have a custom DB layer (mito, postmodern, sxql/sql-table, …) and want CLOG to read/write into it without imposing its own opinions.

Reference patterns:

```lisp
;; Load a row, populate a form, save on submit (clog-dbi):
(let* ((row (query-record *db* :users :where '(= id 42)))
       (form (create-form body)))
  (bind-to-row form row '(("email" . email)
                          ("name"  . full_name)
                          ("role"  . role)))
  (set-on-submit form
                 (lambda (obj) (declare (ignore obj))
                   (sync-from-form form row)
                   (update-record *db* :users row))))
```

The exact symbol names track upstream — open `source/clog-dbi.lisp` for the authoritative API; the upstream tutorial 31 is the worked example.

The DB connection itself is a *connection-data-item* concern, not a global: open one connection per user session at `on-new-window`, store it under `"db"`, close it from a `set-on-close` handler. That avoids "who holds the global connection?" headaches and lets CL-DBI's per-thread cursor reuse work as intended.

## clog-auth — users, sessions, roles

`clog-auth` provides:

- a users table schema and CRUD,
- password hashing (currently bcrypt-style),
- login / logout / signup forms,
- per-page authority guards (`require-authority`),
- "remember me" cookies and session tokens.

Mount it at app start and protect handlers:

```lisp
(defun on-admin (body)
  (clog-auth:require-authority body :admin)   ; redirects to /login if not :admin
  (build-admin-page body))
```

The schema migration is your responsibility — see `source/clog-auth.lisp` for the table layout and adapt to your migration tool of choice (sqitch is a good fit alongside CLOG).

Tutorial 31 ("Database and Authority based websites") shows the end-to-end pattern with `clog-web-dbi` + `clog-auth`. Read it once before assembling your own — the integration points (where to call `require-authority`, where to mount the login route, how `set-on-new-window :path "/login"` interacts with the default boot file) are easier to copy than to reverse-engineer.

## `initialize` — server keyword arguments

```lisp
(initialize 'on-new-window
            :host           "0.0.0.0"        ; default: 127.0.0.1
            :port           8080             ; default: 8080, nil = random
            :static-root    #P"/var/www/static/"
            :boot-file      "/boot.html"     ; default boot page
            :extended-routing  t             ; allow arbitrary :path values
            :long-poll-first   nil           ; t for clients that block WebSocket
            :boot-function     'rewrite-html ; per-request HTML rewriter
            :lack-middleware-list  '()       ; extra Clack middleware
            :ssl              nil
            :ssl-cert-file    nil
            :ssl-key-file     nil)
```

The keyword arguments that actually matter day-to-day:

- **`:host`** — bind to `"0.0.0.0"` to accept LAN connections; leave default for localhost-only dev.
- **`:port`** — pick something stable and stick with it; `nil` chooses a free port (useful in tests, but `clog::*clog-port*` is then the only way to discover it).
- **`:static-root`** — directory served at `/`. Drop CSS, images, downloadable assets here. CLOG serves them with normal HTTP, no WebSocket.
- **`:extended-routing t`** — required if you want `set-on-new-window :path "/foo/bar/baz"` to match deeper-than-one-segment paths. Off by default for safety.
- **`:boot-file`** — the HTML page served on initial GET. Override per-route via `set-on-new-window :boot-file "/custom.html"` if you want the page to render server-side first.
- **`:long-poll-first t`** — falls back to long-polling for clients where WebSocket setup is racy (some old corporate proxies). Slows everything by a few hundred ms; only enable if you've measured a problem.
- **`:boot-function`** — `(lambda (path content) ...)` runs on each delivered boot page; useful for injecting per-route `<meta>` tags for SEO. Tutorial 12 demonstrates the SEO use case.
- **`:lack-middleware-list`** — CLOG runs on Clack; this is your hook for compression, basic auth, custom logging, rate limiting.

Stopping the server: `(shutdown)`. Same image, no restart needed — handy in the REPL.

## Threading: what CLOG does for you, and where you take over

Every `on-new-window` call and every event handler runs in its *own* thread. CLOG hands you concurrency for free per request. Inside a handler:

- `sleep`, `loop`, blocking I/O, long DB queries — all fine. You're not holding any global lock.
- Talking to the browser (`setf` on properties, `create-*`) is thread-safe per connection — CLOG serializes writes onto the WebSocket.
- Talking to the browser from a thread you spawned yourself is also fine, *as long as you check `validp obj`* — once the tab is gone, writes silently no-op but a loop that doesn't notice will burn CPU forever.

Shared in-memory state across connections (a chat room, a global metric) needs your own lock. `bordeaux-threads:make-lock` + `with-lock-held` is the cheap right answer.

The most common "I started a thread and now Bad Things happen" problem is forgetting `validp`. The shape that always works:

```lisp
(bt:make-thread
  (lambda ()
    (loop while (validp display) do
      (sleep 1)
      (setf (text display) (format-current-status)))))
```

If you need server-wide background work (a scheduled job, a feed consumer) that isn't tied to any connection, run it from a thread spawned at app start, *not* from a handler. Handler-spawned threads with no `validp` guard will accumulate across page refreshes.

## Deployment

CLOG apps are normal Lisp images. Production usually means:

1. Build or launch an image that calls `initialize` (without `open-browser`) and then sleeps forever.
2. Run that image under a process supervisor (systemd, runit, S6).
3. Front it with a reverse proxy that handles TLS, gzip, static-file caching, and WebSocket upgrade.

### systemd unit

```ini
[Unit]
Description=My CLOG app
After=network.target

[Service]
ExecStart=/usr/bin/sbcl --non-interactive \
  --eval "(ql:quickload :my-app)" \
  --eval "(my-app:start :port 8081)" \
  --eval "(loop (sleep 360))"
Restart=on-failure
User=clog
WorkingDirectory=/var/lib/my-app

[Install]
WantedBy=multi-user.target
```

The trailing `(loop (sleep 360))` is essential — without it, `--non-interactive` exits after the last `--eval`, which kills the server. Inside the running image, your `(start)` should call `initialize` with `:host "127.0.0.1" :port 8081` so only the proxy can reach it.

For Guix-managed deployments, ship the app as a `package` and let a shepherd service run the image with the same `(loop (sleep 360))` keep-alive idea. The trick is the same regardless of supervisor.

### nginx in front

```nginx
server {
    listen 443 ssl http2;
    server_name app.example.com;

    ssl_certificate     /etc/letsencrypt/live/app.example.com/fullchain.pem;
    ssl_certificate_key /etc/letsencrypt/live/app.example.com/privkey.pem;

    location / {
        proxy_pass         http://127.0.0.1:8081;
        proxy_http_version 1.1;
        proxy_set_header   Upgrade $http_upgrade;
        proxy_set_header   Connection "upgrade";
        proxy_set_header   Host $host;
        proxy_set_header   X-Real-IP $remote_addr;
        proxy_read_timeout 86400s;   # WebSockets are long-lived
        proxy_send_timeout 86400s;
    }
}
```

`Upgrade` + `Connection: upgrade` is the WebSocket handshake — without it CLOG silently falls back to long-polling (slow) or the tab fails to connect entirely. The long read/send timeouts let WebSockets stay open through idle periods; the default `60s` will hang up live UIs.

Apache works the same way with `mod_proxy_wstunnel`:

```apache
ProxyPass        / http://127.0.0.1:8081/
ProxyPassReverse / http://127.0.0.1:8081/
RewriteEngine on
RewriteCond %{HTTP:Upgrade} =websocket [NC]
RewriteRule /(.*) ws://127.0.0.1:8081/$1 [P,L]
```

### Same-origin invariant

WebSockets enforce same-origin: the page that loaded `boot.js` and the WebSocket endpoint must share scheme/host/port from the browser's point of view. With a reverse proxy in front of CLOG this is automatic — both arrive at `https://app.example.com/`. But CDN configurations that serve `/static/` from a different subdomain will silently break the upgrade. Keep WebSockets and the boot page on the same host.

### Running multiple CLOG apps

Two patterns:

- **One image, many routes** — register `set-on-new-window :path "/app-a"` and `:path "/app-b"` and run the whole thing in one Lisp image. Cheapest, but a crash in one app takes down both.
- **One image per app, share nginx** — separate systemd units, separate ports, one nginx `location` block each. Isolation costs ~30 MB of RAM per image.

For production, the second pattern almost always wins. Lisp images are cheap; debugging "why did the analytics dashboard kill our auth service?" is not.

## Health checks and observability

CLOG doesn't ship a `/healthz` endpoint by default. The simplest thing that works: register a route that returns plain text and has no UI.

```lisp
(set-on-new-window
  (lambda (body)
    (setf (inner-html (html-document body)) "ok"))
  :path "/healthz")
```

For real observability — request counts, latency histograms — slot Prometheus middleware into `:lack-middleware-list`. CLOG inherits everything Clack offers there.

## Testing

CLOG's design (server-side state, single-image) makes testing very ordinary:

- **Unit-test handlers as functions** by calling them with a stub `body` or by extracting the handler's logic into a normal function that the handler delegates to.
- **Integration-test the WebSocket** with `cl-clog-test` or a Selenium/Playwright driver against a real browser. Both are heavier; reach for them only when you've broken something the unit tests can't see.

The most useful discipline is to keep `on-new-window` *thin* — it should be UI assembly + handler binding, nothing else. All business logic in normal CL functions you can call from a REPL. The framework rewards this style.
