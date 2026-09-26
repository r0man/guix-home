---
name: guix
description: Expert knowledge for GNU Guix package and service development. Use when creating or modifying Guix packages, system services, home services, or working with a Guix repository checkout. Handles package definitions, build systems, service types, configuration records, and development workflows.
---

# GNU Guix Development Expert

This skill provides comprehensive guidance for working with GNU Guix, including package definitions, system and home services, and development workflows using a Guix repository checkout.

## When to Use This Skill

- Creating new Guix package definitions
- Updating existing packages to new versions
- Defining system services (gnu/services)
- Creating home services (gnu/home/services)
- Working with Guix from a repository checkout
- Debugging package build failures
- Understanding build systems and their options
- Converting packages between old and new styles

## Package Definition Patterns

### Standard Package Structure

Every Guix package follows this basic structure:

```scheme
(define-module (gnu packages category)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages base)
  #:export (package-name))

(define-public package-name
  (package
    (name "package-name")           ; lowercase, dash-separated
    (version "1.0.0")
    (source (origin ...))           ; where to get source
    (build-system gnu-build-system) ; how to build it
    (inputs (list pkg1 pkg2))       ; runtime dependencies
    (native-inputs (list tool1))    ; build-time only tools
    (propagated-inputs (list lib))  ; runtime deps for users
    (arguments (list ...))          ; build system arguments
    (synopsis "One-line description")
    (description "Detailed description in Texinfo format")
    (home-page "https://example.com")
    (license license:gpl3+)))
```

### Module Definition

**Naming Convention:**
- System packages: `(gnu packages CATEGORY)` → `gnu/packages/CATEGORY.scm`
- One package per module or related packages grouped together
- Always use lowercase with dashes

**Required Imports:**
```scheme
(define-module (gnu packages mypackage)
  #:use-module (guix packages)
  #:use-module (guix download)           ; for url-fetch
  #:use-module (guix git-download)       ; for git-fetch
  #:use-module (guix build-system gnu)   ; or other build system
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages base)       ; for common deps
  #:export (mypackage))
```

### Source Origins

**URL Fetch (tarballs, releases):**
```scheme
(source (origin
          (method url-fetch)
          (uri (string-append "https://example.com/package-" version ".tar.gz"))
          (sha256
           (base32 "0ssi1wpaf7plaswqqjwigppsg5fyh99vdlb9kzl7c9lng89ndq1i"))
          (patches (search-patches "package-fix-build.patch"))
          (snippet
           #~(begin
               (use-modules (guix build utils))
               (delete-file-recursively "bundled-lib")))))
```

**Git Fetch (from repository):**
```scheme
(source (origin
          (method git-fetch)
          (uri (git-reference
                (url "https://github.com/example/repo")
                (commit (string-append "v" version))))
          (file-name (git-file-name name version))
          (sha256
           (base32 "..."))))
```

**Getting Source Hashes**

The command you use depends on how the source is fetched. These *must* match,
or the build will fail with a hash mismatch.

**`url-fetch` (tarballs, plain URLs):**
```bash
# Download URL and print its hash
guix download https://example.com/package-1.0.tar.gz

# Hash a file already on disk
guix hash file.tar.gz
```

**`git-fetch` (hashes a directory, not a file):** A `git-fetch` source is
hashed over the *checked-out tree with the `.git` directory removed*. Tarball
hashes will not match — don't mix these up.

```bash
# Option A — clone the exact tag and let guix hash the tree
git clone --depth 1 --branch v1.2.3 https://github.com/example/repo /tmp/repo
guix hash -rx /tmp/repo
# -r  recursive (nar-serialize the tree, which is what git-fetch expects)
# -x  exclude VCS metadata (skips .git even if you forgot to remove it)

# Option B — the "let Guix tell you the hash" idiom (often the easiest)
# Put *any* valid base32 in the sha256 field (e.g. all zeros), then:
guix build -L modules my-package   # or: ./pre-inst-env guix build my-package
# The build will fail with:
#   sha256 hash mismatch for /gnu/store/...
#     expected: 0000000000000000000000000000000000000000000000000000
#     actual:   <the real hash>
# Copy the "actual" value back into the sha256 field. This uses the exact
# same hasher that will later verify the source, so it's the canonical path.
```

Use Option A when you want the hash before running a build; Option B when
you're already iterating on a build and a rebuild is cheap.

### Dependency Management

**Three Types of Inputs:**

1. **`inputs`** - Runtime dependencies (libraries, programs needed when running)
   ```scheme
   (inputs (list libffi glib ncurses))
   ```

2. **`native-inputs`** - Build-time tools (compilers, pkg-config, test frameworks)
   ```scheme
   (native-inputs (list pkg-config autoconf automake check))
   ```

3. **`propagated-inputs`** - Transitive dependencies (headers, libs in public APIs)
   ```scheme
   (propagated-inputs (list python-requests python-urllib3))
   ```

**Using Specific Outputs:**
```scheme
(inputs (list `(,glib "bin")))  ; Use the "bin" output of glib
```

**IMPORTANT:** Modern style uses `(list ...)`, old style uses quasiquote. Prefer list.

### Build Arguments

**Modern Style (G-expressions - PREFERRED):**
```scheme
(arguments
 (list
  #:tests? #t                           ; Run tests (default #t)
  #:parallel-build? #t                  ; Parallel build (default #t)
  #:configure-flags #~(list
    "--enable-foo"
    (string-append "--with-bar=" #$some-input))
  #:make-flags #~(list "VERBOSE=1")
  #:phases
  #~(modify-phases %standard-phases
      (add-after 'unpack 'patch-sources
        (lambda* (#:key inputs #:allow-other-keys)
          (substitute* "Makefile"
            (("/bin/sh") (search-input-file inputs "/bin/sh")))))
      (delete 'configure)
      (replace 'install
        (lambda _
          (install-file "binary" (string-append #$output "/bin")))))))
```

**Old Style (still common):**
```scheme
(arguments
 '(#:tests? #t
   #:phases
   (modify-phases %standard-phases
     (add-after 'install 'fix-paths
       (lambda* (#:key outputs #:allow-other-keys)
         (let ((out (assoc-ref outputs "out")))
           (substitute* (string-append out "/bin/script")
             (("/bin/sh") (which "sh")))))))))
```

### Build Systems

**Most Common Build Systems:**

1. **`gnu-build-system`** - Standard autotools (./configure && make && make install)
2. **`python-build-system`** - Python packages with setup.py
3. **`pyproject-build-system`** - Modern Python (PEP 517/518, poetry, flit, etc.)
4. **`cmake-build-system`** - CMake-based projects
5. **`meson-build-system`** - Meson build system
6. **`cargo-build-system`** - Rust packages
7. **`go-build-system`** - Go packages
8. **`node-build-system`** - Node.js/npm packages
9. **`trivial-build-system`** - Fully custom build process
10. **`copy-build-system`** - Just copy files to output

**GNU Build System Phases (in order):**
```
unpack → patch-source-shebangs → configure → build → check →
install → patch-shebangs → strip → validate-runpath → compress-documentation
```

**Modifying Phases:**
```scheme
(modify-phases %standard-phases
  (add-before 'configure 'set-environment
    (lambda _ (setenv "CC" "gcc")))
  (add-after 'install 'wrap-program
    (lambda* (#:key outputs inputs #:allow-other-keys)
      (wrap-program (string-append #$output "/bin/prog")
        `("PATH" ":" prefix (,(string-append #$some-dep "/bin"))))))
  (replace 'build
    (lambda _ (invoke "make" "all")))
  (delete 'check))  ; Skip tests
```

**Python Build System (Modern) — `pyproject-build-system`:**
```scheme
(build-system pyproject-build-system)
(arguments
 (list
  #:test-flags #~(list "-v"
                       ;; Deselect tests that can't run in the sandbox:
                       "--deselect=tests/test_net.py::test_needs_internet"
                       "--ignore=tests/integration")
  #:build-backend "poetry-core"))  ; or setuptools, flit-core, hatchling, …
(native-inputs
 (list python-pytest
       python-pytest-httpserver         ; test-only deps go HERE
       python-setuptools
       python-wheel))
(propagated-inputs
 (list python-requests))               ; things your Python code imports
```

**Python dependency placement rules — this is the #1 pyproject pitfall:**

- **`native-inputs`** — test runners and test plugins (`python-pytest`,
  `python-pytest-httpserver`, `python-pytest-mock`, `python-hypothesis`),
  build backends (`python-setuptools`, `python-wheel`, `python-hatchling`,
  `python-poetry-core`), code generators, linters used by tests. Anything
  consumed by the `check` phase and not imported at runtime belongs here.
- **`propagated-inputs`** — every Python library the installed package
  *imports at runtime*. Python cannot find unpropagated site-packages, so
  runtime deps must propagate. This is different from C libraries.
- **`inputs`** — non-Python runtime dependencies (C libraries linked by
  the Python extension modules, CLI programs the package shells out to).

A `ModuleNotFoundError` during the `check` phase for a test-only import
(e.g. `No module named 'pytest_httpserver'`) is almost always "I forgot to
add it to `native-inputs`." Diagnose with:

```bash
guix build -L modules my-package --keep-failed
grep -rn pytest_httpserver /tmp/guix-build-*/source   # confirm test-only
guix search pytest-httpserver                          # find the Guix name
```

The Guix name convention is `python-<pypi-name-with-dashes>`. Underscores
in Python import names become dashes in Guix package names:
`pytest_httpserver` → `python-pytest-httpserver`,
`opentelemetry_api` → `python-opentelemetry-api`.

Do not "fix" a missing test dep with `#:tests? #f` or `(delete 'check)`.
Those hide regressions. `#:test-flags` with `--deselect` / `--ignore` is
the right tool only for individual tests that genuinely cannot run in the
sandbox (network, GPU, `/dev` access).

### Common Helpers and Utilities

**In Build Phases:**
```scheme
;; Search for files in inputs
(search-input-file inputs "/bin/sh")
(search-input-directory inputs "/include")

;; Running commands
(invoke "make" "install")        ; Fails if non-zero exit
(system* "make" "test")          ; Returns exit code

;; File operations
(install-file "src/binary" (string-append #$output "/bin"))
(copy-recursively "data" (string-append #$output "/share/data"))
(mkdir-p (string-append #$output "/share/doc"))
(delete-file-recursively "unwanted")

;; Path utilities
(which "sh")                     ; Find in PATH
(dirname "/path/to/file")
(basename "/path/to/file")

;; Substitution (editing files)
(substitute* "config.h"
  (("/usr/local") #$output)
  (("/bin/sh") (which "sh")))

;; Directory context
(with-directory-excursion "subdir"
  (invoke "make"))

;; Environment
(setenv "CC" "gcc")
(getenv "PATH")
```

**Outside Build Phases:**
```scheme
;; Self-reference
this-package

;; File utilities
(local-file "path/to/file.txt")
(plain-file "name" "content")
(computed-file "name" #~(mkdir #$output))

;; Patches
(search-patches "package-fix.patch")  ; looks in gnu/packages/patches/

;; Version manipulation
(version-major+minor "1.2.3")  ; => "1.2"

;; System detection
(%current-target-system)  ; Cross-compilation target
(%current-system)         ; e.g., "x86_64-linux"
(target-64bit?)
(target-aarch64?)
(target-x86-64?)
```

### Package Inheritance

Reuse existing package definitions:

```scheme
(define-public hello-custom
  (package
    (inherit hello)
    (name "hello-custom")
    (arguments
     (substitute-keyword-arguments (package-arguments hello)
       ((#:configure-flags flags)
        #~(cons "--enable-custom" #$flags))
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-after 'install 'install-extra
              (lambda _
                (install-file "extra"
                              (string-append #$output "/bin"))))))))))
```

## Service Definition Patterns

### System Service Type

System services go in `gnu/services/CATEGORY.scm`:

```scheme
(define-module (gnu services myservice)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:export (myservice-configuration
            myservice-service-type))

;; Configuration record
(define-record-type* <myservice-configuration>
  myservice-configuration make-myservice-configuration
  myservice-configuration?
  (package myservice-configuration-package
           (default mypackage))
  (port myservice-configuration-port
        (default 8080))
  (config-file myservice-configuration-config-file
               (default #f)))

;; Generate config file
(define (myservice-config-file config)
  (computed-file "myservice.conf"
    #~(begin
        (use-modules (ice-9 format))
        (call-with-output-file #$output
          (lambda (port)
            (format port "port=~a\n"
                    #$(myservice-configuration-port config)))))))

;; Shepherd service definition
(define (myservice-shepherd-service config)
  (list (shepherd-service
         (documentation "My service daemon")
         (provision '(myservice))
         (requirement '(networking))
         (start #~(make-forkexec-constructor
                   (list #$(file-append
                            (myservice-configuration-package config)
                            "/bin/myservice")
                         "--config"
                         #$(myservice-config-file config))))
         (stop #~(make-kill-destructor)))))

;; Service type definition
(define myservice-service-type
  (service-type
   (name 'myservice)
   (extensions
    (list (service-extension shepherd-root-service-type
                             myservice-shepherd-service)
          (service-extension profile-service-type
                             (lambda (config)
                               (list (myservice-configuration-package config))))))
   (default-value (myservice-configuration))
   (description "Run myservice daemon.")))
```

**Usage in system config:**
```scheme
(service myservice-service-type
         (myservice-configuration
          (port 9000)))
```

### Home Service Type

Home services go in `gnu/home/services/CATEGORY.scm`:

```scheme
(define-module (gnu home services myshell)
  #:use-module (gnu home services)
  #:use-module (gnu services configuration)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:export (home-myshell-configuration
            home-myshell-service-type))

;; Configuration using define-configuration
(define-configuration home-myshell-configuration
  (package
   (package myshell-package)
   "The shell package to use.")
  (aliases
   (alist '())
   "Association list of aliases.")
  (extra-config
   (text-config '())
   "List of strings or file-like objects for config."))

;; Generate files for $HOME
(define (home-myshell-files config)
  (list
   `(".myshellrc"
     ,(mixed-text-file
       "myshellrc"
       #~(string-append
          #$@(home-myshell-configuration-extra-config config))))))

;; Add package to profile
(define (home-myshell-profile config)
  (list (home-myshell-configuration-package config)))

;; Service type
(define home-myshell-service-type
  (service-type
   (name 'home-myshell)
   (extensions
    (list (service-extension home-files-service-type
                             home-myshell-files)
          (service-extension home-profile-service-type
                             home-myshell-profile)))
   (default-value (home-myshell-configuration))
   (description "Install and configure myshell.")))
```

**Usage in home config:**
```scheme
(service home-myshell-service-type
         (home-myshell-configuration
          (extra-config
           (list "alias ll='ls -la'"))))
```

### Home Service Running a User-Level Daemon

When a home service needs to run a long-lived program (a daemon, a watcher,
a user-level webhook receiver), extend `home-shepherd-service-type` rather
than writing shell `exec` lines into a shell rc file. The result is
equivalent to a systemd user service: `herd status` shows it, `herd restart
<name>` works, and respawning is built in.

This is the home analogue of the system service-type pattern above, but uses
*home*-prefixed extension points (`home-shepherd-service-type`,
`home-profile-service-type`) — not `shepherd-root-service-type`, which is
system-level and will look wrong in a home config.

```scheme
(define-module (r0man guix home services mydaemon)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu services shepherd)       ; for shepherd-service
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (r0man guix packages mydaemon)
  #:export (home-mydaemon-configuration
            home-mydaemon-service-type))

(define-record-type* <home-mydaemon-configuration>
  home-mydaemon-configuration make-home-mydaemon-configuration
  home-mydaemon-configuration?
  (package     home-mydaemon-package
               (default mydaemon))
  (port        home-mydaemon-port
               (default 7070))
  (config-file home-mydaemon-config-file
               (default #f)))                   ; file-like or #f

(define (home-mydaemon-shepherd-services config)
  (list
   (shepherd-service
    (documentation "Run the mydaemon user service.")
    (provision '(mydaemon))
    (respawn? #t)
    (start #~(make-forkexec-constructor
              (append
               (list #$(file-append
                         (home-mydaemon-package config)
                         "/bin/mydaemon")
                     "--port" #$(number->string
                                  (home-mydaemon-port config)))
               (if #$(home-mydaemon-config-file config)
                   (list "--config"
                         #$(home-mydaemon-config-file config))
                   '()))
              #:log-file (string-append
                          (or (getenv "XDG_STATE_HOME")
                              (string-append (getenv "HOME")
                                             "/.local/state"))
                          "/mydaemon.log")))
    (stop #~(make-kill-destructor)))))

(define (home-mydaemon-profile-packages config)
  (list (home-mydaemon-package config)))

(define home-mydaemon-service-type
  (service-type
   (name 'home-mydaemon)
   (extensions
    (list (service-extension home-shepherd-service-type
                             home-mydaemon-shepherd-services)
          (service-extension home-profile-service-type
                             home-mydaemon-profile-packages)))
   (default-value (home-mydaemon-configuration))
   (description "Run mydaemon as a user-level shepherd service.")))
```

**Key points that trip people up:**

- Use **`file-append`** to compute the binary path at build time
  (`(file-append pkg "/bin/mydaemon")`). Do not hardcode a store path and
  do not call `which`.
- `make-forkexec-constructor` does **not** inherit the parent shell's
  environment. If the daemon needs specific env vars, pass
  `#:environment-variables` explicitly with a list of `"KEY=value"`
  strings. A common pitfall is assuming `PATH` is set — it often isn't.
- The `start`/`stop` gexps run in the shepherd process at service-start
  time. Values from `config` must be lowered with `#$` *inside* the gexp,
  which is why the example `let`-binds at the outer level and then splices.
- Logs go somewhere persistent by convention — `$XDG_STATE_HOME` or
  `~/.local/state/<name>.log` is a reasonable default.
- A `home-<name>-profile-packages` helper makes the package available on
  `$PATH`, which is almost always what you also want when you're shipping
  a daemon (so the user can invoke the binary directly too).

### Service Extensions

Common extension points:

**System Services:**
- `shepherd-root-service-type` - Add shepherd services (daemons)
- `profile-service-type` - Add packages to system profile
- `etc-service-type` - Add files to /etc
- `activation-service-type` - Run code at system activation

**Home Services:**
- `home-files-service-type` - Add files to $HOME
- `home-xdg-configuration-files-service-type` - Add files to $XDG_CONFIG_HOME
- `home-profile-service-type` - Add packages to home profile
- `home-run-on-change-service-type` - Run code when config changes

## Working in a Personal Channel

Most users doing real Guix work day-to-day are *not* editing a checkout of
`guix.git`. They are working inside a personal channel or a Guix Home
repository that ships its own `modules/` directory with package and service
definitions. In that situation you do **not** need `./pre-inst-env` — you
just point Guix at your modules with `-L`.

**The key flag is `-L <path>`** (or `--load-path=<path>`), which adds a
directory to Guile's module search path for that one invocation. The
working tree is used directly, so changes are picked up immediately — no
`guix pull`, no rebuild of Guix itself.

```bash
# From the channel repo root — build a package from your local source
guix build -L modules my-package

# Keep the build tree on failure for inspection
guix build -L modules --keep-failed my-package

# Lint it using your local checkout
guix lint  -L modules my-package

# Dry-run the whole home environment against the local tree
# (catches module-load errors, unbound variables, and derivation-graph issues)
guix home -L modules --dry-run reconfigure \
  modules/r0man/guix/home/environments/m1.scm

# Drop into a REPL with the local modules visible
guix repl -L modules
,use (r0man guix packages task-management)
(package-version my-package)
```

If a package isn't reachable by bare name (e.g. because the CLI can't
auto-discover it), build it by an expression instead:

```bash
guix build -L modules \
  -e '(@ (r0man guix packages task-management) my-package)'
```

**When to use `./pre-inst-env` vs `-L modules`:**

- `./pre-inst-env` — you're inside a checkout of `guix.git` itself and are
  editing packages in upstream Guix (`gnu/packages/*.scm`). You need a
  built Guix to run.
- `-L modules` — you're in a personal channel or Guix Home repo whose
  modules sit under `modules/` in the working tree. Nothing to compile
  first; the installed Guix just loads your modules too.

Use `-L modules` unless you're specifically contributing to upstream Guix.

**Updating a package in a personal channel** (the common workflow):

1. Edit `version` and `sha256` in the package definition.
2. Get the new hash. For `git-fetch` sources, see "Getting Source Hashes"
   above — the easiest path is Option B: drop in a bogus `(base32 "00…")`
   hash, run `guix build -L modules <pkg>`, and copy the `actual:` line
   out of the hash-mismatch error.
3. `guix build -L modules <pkg>` until it builds cleanly.
4. `guix lint -L modules <pkg>` for style/metadata issues.
5. `guix home -L modules --dry-run reconfigure …environments/<host>.scm`
   to verify dependents still resolve.
6. Commit, then run a real `guix home reconfigure` on the affected host.

## Working with Guix Repository

### Setup and Build

```bash
# Clone repository
git clone https://git.savannah.gnu.org/git/guix.git
cd guix

# Bootstrap and configure
./bootstrap
./configure --localstatedir=/var

# Build
make -j$(nproc)

# Install git hooks (for contributors)
make install-git-hooks
```

### Using pre-inst-env

The `./pre-inst-env` wrapper lets you test changes without installing:

```bash
# Build a package from your checkout
./pre-inst-env guix build hello

# Install a package you just defined
./pre-inst-env guix package -i my-new-package

# Start a REPL with your changes
./pre-inst-env guix repl

# Lint your package
./pre-inst-env guix lint my-package

# Check for updates
./pre-inst-env guix refresh my-package
```

**What pre-inst-env does:**
- Sets GUILE_LOAD_PATH to use checkout modules
- Sets PATH to include build directory scripts
- Prevents loading installed Guix modules

### Testing Packages

```bash
# Build with options
./pre-inst-env guix build hello --no-grafts --keep-failed

# Build from a file
guix build -f my-package.scm

# Enter build environment
./pre-inst-env guix shell -D hello

# Build and check size
./pre-inst-env guix size hello

# View dependency graph
./pre-inst-env guix graph hello | dot -Tpng > graph.png
```

### Using guix repl

Interactive Scheme REPL for package development:

```scheme
$ ./pre-inst-env guix repl

;; Import modules
,use (gnu packages base)
,use (guix packages)
,use (guix gexp)

;; Inspect a package
hello
(package-name hello)
(package-version hello)
(package-arguments hello)

;; Build a package
,build hello

;; Lower to derivation
,lower hello

;; Work with transformations
(package-with-c-toolchain hello `(("toolchain" ,gcc-toolchain)))
```

### Module Loading

```scheme
;; Import entire module
(use-modules (gnu packages base))

;; Import with prefix
(use-modules ((guix licenses) #:prefix license:))

;; Import specific bindings
(use-modules ((gnu packages gcc) #:select (gcc)))

;; Autoload (lazy loading)
#:autoload (gnu packages gcc) (gcc)
```

## Common Workflows

### Workflow 1: Adding a New Package

1. **Find the right module** in `gnu/packages/` (e.g., `gnu/packages/networking.scm`)

2. **Add necessary imports:**
   ```scheme
   #:use-module (guix packages)
   #:use-module (guix download)
   #:use-module (guix build-system gnu)
   #:use-module ((guix licenses) #:prefix license:)
   ```

3. **Define the package:**
   ```scheme
   (define-public my-package
     (package
       (name "my-package")
       (version "1.0.0")
       (source (origin ...))
       ...))
   ```

4. **Get the source hash:**
   ```bash
   guix download https://example.com/my-package-1.0.0.tar.gz
   ```

5. **Test the build:**
   ```bash
   ./pre-inst-env guix build my-package
   ```

6. **Run linter:**
   ```bash
   ./pre-inst-env guix lint my-package
   ```

7. **Test installation:**
   ```bash
   ./pre-inst-env guix package -i my-package
   ```

### Workflow 2: Updating a Package

1. **Edit the version:**
   ```scheme
   (version "2.0.0")
   ```

2. **Update URI if needed:**
   ```scheme
   (uri (string-append "..." version ".tar.gz"))
   ```

3. **Get new hash:**
   ```bash
   guix download https://example.com/package-2.0.0.tar.gz
   ```

4. **Update sha256:**
   ```scheme
   (sha256 (base32 "NEW-HASH-HERE"))
   ```

5. **Test build:**
   ```bash
   ./pre-inst-env guix build package-name
   ```

6. **Check for changes in dependencies or build process**

### Workflow 3: Creating a Service

1. **Create module file:**
   - System: `gnu/services/myservice.scm`
   - Home: `gnu/home/services/myservice.scm`

2. **Define configuration record:**
   ```scheme
   (define-record-type* <myservice-configuration> ...)
   ```

3. **Create helper functions:**
   - Config file generation
   - Shepherd service definition
   - File generation

4. **Define service-type:**
   ```scheme
   (define myservice-service-type
     (service-type
      (name 'myservice)
      (extensions ...)
      (default-value ...)
      (description "...")))
   ```

5. **Test in a config:**
   ```bash
   guix system reconfigure config.scm  # system service
   guix home reconfigure home.scm       # home service
   ```

### Workflow 4: Debugging Build Failures

1. **Build with --keep-failed:**
   ```bash
   ./pre-inst-env guix build my-package --keep-failed
   ```

2. **Check the build log** for error messages

3. **Enter build environment:**
   ```bash
   ./pre-inst-env guix shell -D my-package
   cd /tmp/guix-build-my-package-1.0.0-0  # from --keep-failed
   ```

4. **Manually reproduce the failed step**

5. **Add diagnostic output in phases:**
   ```scheme
   (add-before 'build 'debug
     (lambda _
       (format #t "Current directory: ~a\n" (getcwd))
       (system* "ls" "-la")))
   ```

6. **Common issues and fixes:**
   - Missing dependencies → Add to inputs/native-inputs
   - Hardcoded paths → Use substitute* to fix
   - Test failures → Disable tests or fix environment
   - Wrong build system → Try different build-system

## Common Pitfalls and Solutions

### Hash Mismatch
**Problem:** Source hash doesn't match
**Solution:** Use `guix download URL` to get correct hash, copy the base32 hash

### Missing Dependencies
**Problem:** Build fails with "command not found" or missing libraries
**Solution:** Add missing tools to `native-inputs`, libraries to `inputs`

### Test Failures
**Problem:** Tests fail in sandboxed build environment
**Solution 1:** Disable specific tests
```scheme
(add-before 'check 'disable-failing-tests
  (lambda _
    (substitute* "tests/test_network.py"
      (("test_network") "disabled_test_network"))))
```
**Solution 2:** Disable all tests (last resort)
```scheme
(arguments (list #:tests? #f))
```

### Hardcoded Absolute Paths
**Problem:** Program has /usr or /bin paths
**Solution:** Use substitute* to replace them
```scheme
(substitute* "script.sh"
  (("/bin/sh") (which "sh"))
  (("/usr/local") #$output))
```

### Build Directory Modified
**Problem:** "source directory modified" error
**Solution:** Use out-of-source build
```scheme
(arguments (list #:out-of-source? #t))
```

### Phase Ordering
**Problem:** Phase runs in wrong order
**Solution:** Use add-before, add-after, replace carefully
```scheme
(add-after 'unpack 'patch-before-configure ...)
(add-before 'build 'set-flags ...)
```

## Important Commands

### Package Development
```bash
guix build package-name              # Build package
guix build -f file.scm               # Build from file
guix build --check package           # Rebuild to verify reproducibility
guix build --keep-failed package     # Keep build directory on failure
guix build --no-grafts package       # Disable security grafts

guix lint package-name               # Check for issues
guix refresh package-name            # Check for updates
guix size package-name               # Check closure size
guix graph package-name              # Show dependencies

guix shell -D package                # Enter development environment
guix shell package -- command        # Run command with package

guix hash file.tar.gz                # Get hash of file
guix download URL                    # Download and hash
```

### Testing
```bash
make check                           # Run test suite
./pre-inst-env guix build hello      # Test with local changes
guix repl                            # Interactive testing
```

## Key References

When working from a Guix repository checkout, refer to these locations:

- **Package definitions:** `gnu/packages/`
- **System services:** `gnu/services/`
- **Home services:** `gnu/home/services/`
- **Build systems:** `guix/build-system/`
- **Build utilities:** `guix/build/utils.scm`
- **Patches:** `gnu/packages/patches/`
- **Manual:** `doc/guix.texi`
- **Cookbook:** `doc/guix-cookbook.texi`
- **Contributing:** `doc/contributing.texi`

## Best Practices

1. **Use modern G-expression style** for arguments (list with #~)
2. **Keep packages focused** - one package per purpose
3. **Follow naming conventions** - lowercase with dashes
4. **Add patches to patches/** directory with search-patches
5. **Test thoroughly** - build, lint, install
6. **Write clear descriptions** - synopsis is one line, description is detailed
7. **Use appropriate build systems** - don't default to gnu-build-system
8. **Minimize inputs** - only include necessary dependencies
9. **Propagate sparingly** - only when truly needed by users
10. **Document unusual choices** - add comments for non-obvious decisions
11. **Avoid make clean/distclean** - Rebuilding the Guix repo takes time

## Getting Help

When stuck, use these resources:

```bash
# View package definition
./pre-inst-env guix edit package-name

# Search for similar packages
./pre-inst-env guix search keyword

# Check package dependencies
./pre-inst-env guix graph package-name

# View build log
guix build --log-file package-name

# Ask on IRC: #guix on libera.chat
# Mailing list: help-guix@gnu.org
```
