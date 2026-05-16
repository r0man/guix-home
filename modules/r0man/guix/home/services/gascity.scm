(define-module (r0man guix home services gascity)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lsof)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages tmux)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages web)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (r0man guix home services gascity-toml)
  #:use-module (r0man guix packages golang-dolthub)
  #:use-module (r0man guix packages task-management)
  #:export (gascity-rig-configuration
            gascity-rig-configuration?
            gascity-rig-path
            gascity-rig-name
            gascity-rig-git-url
            gascity-rig-branch
            gascity-rig-depth
            gascity-rig-prefix

            gascity-pack-configuration
            gascity-pack-configuration?
            gascity-pack-name
            gascity-pack-source
            gascity-pack-ref
            gascity-pack-path

            gascity-agent-configuration
            gascity-agent-configuration?
            gascity-agent-name
            gascity-agent-provider
            gascity-agent-command

            gascity-city-configuration
            gascity-city-configuration?
            gascity-city-path
            gascity-city-name
            gascity-city-rigs
            gascity-city-managed?
            gascity-city-provider
            gascity-city-bootstrap-profile
            gascity-city-init-from
            gascity-city-packs
            gascity-city-agents
            gascity-city-providers
            gascity-city-workspace-suspended?
            gascity-city-extra-toml

            gascity-dolt-configuration
            gascity-dolt-configuration?
            gascity-dolt-user-name
            gascity-dolt-user-email

            gascity-supervisor-configuration
            gascity-supervisor-configuration?
            gascity-supervisor-name
            gascity-supervisor-gc-home
            gascity-supervisor-beads-provider
            gascity-supervisor-providers
            gascity-supervisor-cities
            gascity-supervisor-supervisor-port
            gascity-supervisor-dashboard?
            gascity-supervisor-dashboard-port

            home-gascity-configuration
            home-gascity-configuration?
            home-gascity-packages
            home-gascity-dolt
            home-gascity-supervisors
            home-gascity-service-type))

;;; Commentary:
;;;
;;; Multi-supervisor Gas City home service.
;;;
;;; A single home-gascity-configuration value carries one or more
;;; <gascity-supervisor-configuration> records in (supervisors …).
;;; Each supervisor owns its own GC_HOME, its own (gc supervisor run)
;;; shepherd service, and its own one-shot init service that lays out
;;; cities and rigs.  Multiple supervisors share one host but bind
;;; disjoint API ports and live in separate GC_HOME directories —
;;; useful for development isolation (default 'main' supervisor +
;;; ad-hoc test supervisors).
;;;
;;; Per-supervisor shepherd provisions are uniformly suffixed:
;;; gascity-supervisor-<NAME>, gascity-init-<NAME>, optionally
;;; gascity-dashboard-<NAME>.  No special unsuffixed name; the default
;;; supervisor (name 'default) provisions gascity-supervisor-default.
;;;
;;; Dolt identity is host-level, stored on the configuration as a
;;; top-level <gascity-dolt-configuration> record in (dolt …).  It is
;;; NOT per-supervisor: ~/.dolt/config_global.json is a single file
;;; per user, and a per-supervisor dolt identity would be a schema
;;; lie.  The defaults ("Gas City" / "gascity@localhost") preserve
;;; current behavior — overriding via (dolt (gascity-dolt-configuration
;;; (user-name …) (user-email …))) is strongly recommended in any
;;; real deployment that lacks home-git-service-type.
;;;
;;; Activation is a single home-activation-service-type gexp that:
;;;
;;;   1. Seeds ~/.dolt/config_global.json with the identity from
;;;      (home-gascity-dolt config), but only when at least one
;;;      supervisor uses (beads-provider 'bd) and neither
;;;      ~/.dolt/config_global.json nor ~/.gitconfig already exists
;;;      (dolt falls back to git config).
;;;
;;;   2. Per-supervisor: mkdir GC_HOME and city dirs; write
;;;      <gc-home>/supervisor.toml, <gc-home>/cities.toml, and managed
;;;      <city>/city.toml atomically (via .tmp + rename) so the
;;;      supervisor's patrol-tick os.ReadFile cannot see half-written
;;;      TOML; drop <city>/.guix-home-managed sidecar markers; for
;;;      file-beads cities, drop <city>/.gc/file-beads-layout and
;;;      <city>/.gc/beads.json mirroring upstream's
;;;      bootstrapScopedFileProviderCityFS.
;;;
;;; If you previously used `gc supervisor install' (manual launchd or
;;; systemd-user setup on macOS or foreign-distro Linux), run `gc
;;; supervisor uninstall' once by hand before switching to this
;;; service.
;;;
;;; The activation marker for "this city is guix-home managed" is the
;;; sidecar file <city>/.guix-home-managed — NOT a comment in
;;; city.toml.  TOML round-trips through gascity's BurntSushi encoder
;;; strip comments (internal/config/config.go:2712-2721), so any `gc
;;; rig add' or `gc agent add' would erase an in-file sentinel after
;;; one mutation.  The marker file is invisible to gascity walkers
;;; (`gc doctor', `gc rig add', `gc supervisor reload' do not scan for
;;; unknown dotfiles).
;;;
;;; The rendered [[rigs]] entries match what `gc rig add' would
;;; produce (name = basename or explicit name; path = abs path; prefix
;;; = derive-beads-prefix of name).  Combined with `gc rig add's
;;; re-add fast-path (cmd_rig.go:243-268), the call from gascity-init
;;; runs side effects (hooks/routes/templates) without rewriting
;;; city.toml.
;;;
;;; GC_HOME exported into the user's shell points at the first
;;; supervisor's gc-home by default.  Override with (default-gc-home
;;; ".gc-other") at the home-gascity-configuration level when you have
;;; multiple supervisors and want the shell to drive one that is not
;;; the first; the value must match one of the supervisors' (gc-home
;;; …) strings or reconfigure fails.  Multi-supervisor setups can also
;;; `GC_HOME=$HOME/.gc-other gc supervisor status' for ad-hoc reach;
;;; we intentionally do not emit multiple GC_HOME values (downstream
;;; tools read a single env var).
;;;
;;; Code:


;;;
;;; Record types.
;;;

(define-record-type* <gascity-rig-configuration>
  gascity-rig-configuration make-gascity-rig-configuration
  gascity-rig-configuration?
  (path    gascity-rig-path
           (description "Filesystem path to the rig checkout — absolute or
relative to the containing city directory."))
  (name    gascity-rig-name
           (default #f)
           (description "Optional explicit rig name.  Defaults to (basename
PATH).  Within a single city, derived rig names must be unique — `gc rig
add' matches by name only (cmd_rig.go:243-268) and exits 1 on the second
entry."))
  (git-url gascity-rig-git-url
           (default #f)
           (description "Optional git remote.  When set and PATH does not
exist, gascity-init runs 'git clone <git-url> <path>'."))
  (branch  gascity-rig-branch
           (default #f)
           (description "Optional branch passed to git clone via --branch.
Clone-time only; does not appear in the rendered [[rigs]] block (gascity's
Rig struct has no branch field, internal/config/config.go:399-445)."))
  (depth   gascity-rig-depth
           (default 1)
           (description "Optional --depth value for git clone.  Clone-time
only; does not appear in the rendered [[rigs]] block."))
  (prefix  gascity-rig-prefix
           (default #f)
           (description "Optional explicit beads prefix.  Defaults to
(derive-beads-prefix NAME) so the rendered [[rigs]] entry matches what `gc
rig add' would write itself, keeping its re-add fast-path on the
config-write-skipping branch.")))

(define-record-type* <gascity-pack-configuration>
  gascity-pack-configuration make-gascity-pack-configuration
  gascity-pack-configuration?
  (name   gascity-pack-name
          (description "Pack table key — rendered as [packs.<name>]."))
  (source gascity-pack-source
          (description "Git remote for the pack."))
  (ref    gascity-pack-ref
          (default #f)
          (description "Optional git ref (branch/tag/commit)."))
  (path   gascity-pack-path
          (default #f)
          (description "Optional subdirectory inside the pack repo.")))

(define-record-type* <gascity-agent-configuration>
  gascity-agent-configuration make-gascity-agent-configuration
  gascity-agent-configuration?
  (name     gascity-agent-name
            (description "Agent name."))
  (provider gascity-agent-provider
            (description "Provider preset name (string)."))
  (command  gascity-agent-command
            (default #f)
            (description "Optional start_command override.")))

(define-record-type* <gascity-city-configuration>
  gascity-city-configuration make-gascity-city-configuration
  gascity-city-configuration?
  (path                 gascity-city-path
                        (description "Absolute filesystem path to the city
directory."))
  (name                 gascity-city-name
                        (default #f)
                        (description "Optional explicit name; otherwise the
path's basename is used."))
  (rigs                 gascity-city-rigs
                        (default '())
                        (description "List of <gascity-rig-configuration>
records."))
  (managed?             gascity-city-managed?
                        (default #t)
                        (description "When true (default), city.toml is
rendered from this record on every reconfigure and a sidecar
<city>/.guix-home-managed marker is dropped.  When false, the city is
'gc init'ed once and never overwritten — runtime mutations like `gc agent
add' persist.  Out-of-band edits to 'city.toml' — including those made
by 'gc rig add' (which writes directly via writeCityConfigForEditFS at
cmd_rig.go:494, bypassing the supervisor API) — are lost on the next
'guix home reconfigure' when 'managed?' is #t.  Add rigs to this record
in Scheme instead."))
  (provider             gascity-city-provider
                        (default #f)
                        (description "Optional [workspace] provider value
for the city.  Distinct from the inherited beads provider; this is the
agent transport (claude/codex/gemini)."))
  (bootstrap-profile    gascity-city-bootstrap-profile
                        (default 'minimal)
                        (description "Reserved for future use; currently
unused by the rendered city.toml."))
  (init-from            gascity-city-init-from
                        (default #f)
                        (description "Optional template-city directory: a
file-like (e.g. (file-append gascity-next \"/share/gascity/examples/
gastown\")) or a string path.  When set and the city's city.toml does
not yet exist, gascity-init bootstraps the city ONCE via `gc init
--from <init-from> <path>', deep-copying the template's
packs/agents/assets, then restores owner-write on the copied surface
(templates shipped from the read-only Guix store land 0444; the live
.beads/.gc dolt runtime is left untouched).  Requires (managed? #f): a
managed city.toml render would clobber the copied template, so the two
are mutually exclusive and validated at configuration time.  Caveat:
`gc init --from' re-stamps the workspace name into city.toml WHILE the
just-copied file is still 0444, so that one rewrite logs a harmless
\"permission denied\".  This is a no-op when the template's [workspace]
name already equals (basename PATH) — the recommended setup, and the
case for the bundled examples (examples/gastown → ~/cities/gastown).
If you point (init-from …) at a template whose workspace name differs
from the target directory's basename, the name will not be
re-stamped."))
  (packs                gascity-city-packs
                        (default '())
                        (description "List of <gascity-pack-configuration>
records — rendered as [packs.<name>] tables."))
  (agents               gascity-city-agents
                        (default '())
                        (description "List of <gascity-agent-configuration>
records — rendered as [[agent]] entries."))
  (city-providers       gascity-city-providers
                        (default '())
                        (description "List of (name . alist) pairs rendered
as [providers.<name>] tables."))
  (workspace-suspended? gascity-city-workspace-suspended?
                        (default #f)
                        (description "When true, sets [workspace] suspended
= true."))
  (extra-toml           gascity-city-extra-toml
                        (default "")
                        (description "Raw TOML string appended verbatim to
the rendered city.toml.  Escape hatch for sections this service does not
model (chat_sessions, dolt overrides, etc.).")))

;; Host-level dolt identity.  Seeded into ~/.dolt/config_global.json
;; once per host by the activation gexp (only when at least one
;; supervisor uses (beads-provider 'bd) AND neither
;; ~/.dolt/config_global.json nor ~/.gitconfig already exists).
;;
;; WARNING: the literal defaults ("Gas City" / "gascity@localhost")
;; WILL end up in ~/.dolt/config_global.json for users without
;; home-git-service-type.  Overriding is strongly recommended in any
;; real deployment:
;;
;;   (dolt (gascity-dolt-configuration
;;          (user-name "Ada Lovelace")
;;          (user-email "ada@example.com")))
(define-record-type* <gascity-dolt-configuration>
  gascity-dolt-configuration make-gascity-dolt-configuration
  gascity-dolt-configuration?
  (user-name  gascity-dolt-user-name
              (default "Gas City")
              (description "Identity written to
~/.dolt/config_global.json once per host (only when any supervisor uses
'bd AND neither ~/.dolt/config_global.json nor ~/.gitconfig exists).
The default leaks into the global dolt config for users without
home-git-service-type — overriding is strongly recommended."))
  (user-email gascity-dolt-user-email
              (default "gascity@localhost")
              (description "Companion email for the seeded dolt
identity.  Overriding is strongly recommended; see user-name.")))

(define-record-type* <gascity-supervisor-configuration>
  gascity-supervisor-configuration make-gascity-supervisor-configuration
  gascity-supervisor-configuration?
  (name             gascity-supervisor-name
                    (default 'default)
                    (description "Symbol used in shepherd provision names —
sanitised to [a-z0-9-]+ so the symbol gascity-supervisor-<NAME> is a
valid scheme symbol."))
  (gc-home          gascity-supervisor-gc-home
                    (default ".gc")
                    (description "Supervisor runtime directory relative to
$HOME.  Two supervisors must not share a path — they would race on
<gc-home>/supervisor.lock."))
  (beads-provider   gascity-supervisor-beads-provider
                    (default 'bd)
                    (description "'bd or 'file.  When 'file, gascity-init
drops <city>/.gc/file-beads-layout and <city>/.gc/beads.json mirroring
upstream's bootstrapScopedFileProviderCityFS (cmd_init.go:934).  When no
supervisor uses 'bd, dolt and beads-next are filtered from the profile and
the dolt identity seed is skipped."))
  (providers        gascity-supervisor-providers
                    (default '())
                    (description "List of provider opt-ins.  Each entry is
either a bare symbol ('claude resolves to claude-code from (r0man guix
packages claude); 'codex / 'gemini are reserved warn-only) or a
(<sym> #:package <pkg>) two-element list for user-supplied packages."))
  (cities           gascity-supervisor-cities
                    (default '())
                    (description "List of <gascity-city-configuration>
records belonging to this supervisor."))
  (supervisor-port  gascity-supervisor-supervisor-port
                    (default 8372)
                    (description "Port written into <gc-home>/supervisor.toml
[supervisor].port — pre-empts gascity's seedIsolatedSupervisorConfig
auto-seed (internal/supervisor/config.go:212-239)."))
  (dashboard?       gascity-supervisor-dashboard?
                    (default #f)
                    (description "When true, an additional one-shot
gascity-dashboard-<NAME> shepherd service runs `gc dashboard serve --port
<DASHBOARD-PORT>'."))
  (dashboard-port   gascity-supervisor-dashboard-port
                    (default 8080)
                    (description "HTTP port for the dashboard.  Validation
rejects collisions across supervisors with (dashboard? #t).")))

(define-record-type* <home-gascity-configuration>
  home-gascity-configuration make-home-gascity-configuration
  home-gascity-configuration?
  (packages    home-gascity-packages
               (default %home-gascity-default-packages)
               (description "Profile-level packages.  When no supervisor
uses (beads-provider 'bd) the dolt and beads-next packages are filtered
out; provider opt-ins resolved from (providers …) are appended."))
  (dolt        home-gascity-dolt
               (default (gascity-dolt-configuration))
               (description "Host-level dolt identity record.  See
<gascity-dolt-configuration> — defaults match the prior behaviour, but
overriding is strongly recommended in any real deployment."))
  (supervisors home-gascity-supervisors
               (description "Required, non-empty list of
<gascity-supervisor-configuration> records.  Empty raises a configuration
error before any derivation work."))
  (default-gc-home home-gascity-default-gc-home
                   (default #f)
                   (description "Optional gc-home string (relative to
$HOME, e.g. \".gc-other\") to export as GC_HOME.  Must match one of the
supervisors' (gc-home …) values — mismatches raise a configuration
error at reconfigure time.  When #f (default), GC_HOME falls back to
the first supervisor's gc-home; set this when you have multiple
supervisors and want the shell to drive one that is not the first.")))


;;;
;;; Default profile package list.  Provider-specific packages and
;;; dolt/beads-next are filtered or appended at service-type extension
;;; time based on the configured instances.
;;;

(define %home-gascity-default-packages
  (list gascity-next dolt beads-next
        coreutils guix
        tmux git grep jq procps sed util-linux lsof))


;;;
;;; Validation.
;;;

(define (gascity-sanitize-supervisor-name sym)
  "Lowercase SYM (a symbol) and drop any character outside [a-z0-9-]."
  (let* ((s (symbol->string sym))
         (chars (string->list (string-downcase s)))
         (kept (filter (lambda (c)
                         (or (char-numeric? c)
                             (and (char>=? c #\a) (char<=? c #\z))
                             (eqv? c #\-)))
                       chars)))
    (string->symbol (list->string kept))))

(define (gascity-supervisor-provision-symbol prefix supervisor)
  "Build a shepherd provision symbol PREFIX-<sanitized-name>."
  (symbol-append prefix '-
                 (gascity-sanitize-supervisor-name
                  (gascity-supervisor-name supervisor))))

(define (gascity-rig-derived-name rig)
  "Return the canonical name for RIG: explicit (name …) when set;
otherwise (basename PATH)."
  (or (gascity-rig-name rig)
      (basename (gascity-rig-path rig))))

(define (gascity-validate-supervisors! supervisors)
  "Raise &error before any derivation work when SUPERVISORS violates one of
the four uniqueness invariants:
  (a) gc-home paths must be unique across supervisors.
  (b) supervisor-port values must be unique.
  (c) dashboard-port values must be unique among (dashboard? #t) supervisors.
  (d) within each city, derived rig names (basename-of-path or explicit
      (name …)) must be unique.
  (e) a city with (init-from …) set must also have (managed? #f) — a
      managed city.toml render would clobber the copied template.
Also rejects duplicate sanitized supervisor names."
  (when (null? supervisors)
    (error 'home-gascity-configuration
           "supervisors must be a non-empty list of \
<gascity-supervisor-configuration> records"))
  ;; Sanitized name uniqueness.
  (let* ((names (map (lambda (s)
                       (gascity-sanitize-supervisor-name
                        (gascity-supervisor-name s)))
                     supervisors))
         (dups  (filter (lambda (n)
                          (> (length (filter (lambda (m) (eq? m n)) names))
                             1))
                        names)))
    (unless (null? dups)
      (error 'home-gascity-configuration
             "supervisors have colliding sanitized names" (delete-duplicates dups))))
  ;; (a) gc-home uniqueness.
  (let* ((paths (map gascity-supervisor-gc-home supervisors))
         (dups  (filter (lambda (p)
                          (> (length (filter (lambda (q) (string=? p q))
                                             paths))
                             1))
                        paths)))
    (unless (null? dups)
      (error 'home-gascity-configuration
             "two supervisors share the same gc-home path"
             (delete-duplicates dups))))
  ;; (b) supervisor-port uniqueness.
  (let* ((ports (map gascity-supervisor-supervisor-port supervisors))
         (dups  (filter (lambda (p)
                          (> (length (filter (lambda (q) (= p q)) ports))
                             1))
                        ports)))
    (unless (null? dups)
      (error 'home-gascity-configuration
             "two supervisors share the same supervisor-port"
             (delete-duplicates dups))))
  ;; (c) dashboard-port uniqueness across enabled supervisors.
  (let* ((dashboards (filter gascity-supervisor-dashboard? supervisors))
         (ports (map gascity-supervisor-dashboard-port dashboards))
         (dups  (filter (lambda (p)
                          (> (length (filter (lambda (q) (= p q)) ports))
                             1))
                        ports)))
    (unless (null? dups)
      (error 'home-gascity-configuration
             "two dashboard-enabled supervisors share the same dashboard-port"
             (delete-duplicates dups))))
  ;; (d) within each city, derived rig names must be unique.
  (for-each
   (lambda (supervisor)
     (for-each
      (lambda (city)
        (let* ((rigs (gascity-city-rigs city))
               (names (map gascity-rig-derived-name rigs))
               (dups (filter (lambda (n)
                               (> (length
                                   (filter (lambda (m) (string=? m n))
                                           names))
                                  1))
                             names)))
          (unless (null? dups)
            (error 'home-gascity-configuration
                   "within a city, two rigs share the same derived name"
                   (gascity-city-path city)
                   (delete-duplicates dups)))
          ;; (e) init-from implies (managed? #f).
          (when (and (gascity-city-init-from city)
                     (gascity-city-managed? city))
            (error 'home-gascity-configuration
                   "a city with (init-from …) must also set (managed? #f)"
                   (gascity-city-path city)))))
      (gascity-supervisor-cities supervisor)))
   supervisors))

(define (home-gascity-validate-configuration! config)
  "Run all configuration-level invariants before any derivation work:
the supervisor uniqueness rules in 'gascity-validate-supervisors!', plus
the (default-gc-home …) match against the supervisors' gc-home values."
  (let ((supervisors (home-gascity-supervisors config))
        (explicit    (home-gascity-default-gc-home config)))
    (gascity-validate-supervisors! supervisors)
    (when explicit
      (unless (string? explicit)
        (error 'home-gascity-configuration
               "default-gc-home must be a string (gc-home relative to $HOME) or #f"
               explicit))
      (unless (find (lambda (s)
                      (string=? (gascity-supervisor-gc-home s) explicit))
                    supervisors)
        (error 'home-gascity-configuration
               "default-gc-home does not match any supervisor's gc-home"
               explicit
               (map gascity-supervisor-gc-home supervisors))))))


;;;
;;; Provider opt-in resolution.
;;;
;;; Each provider entry is either a bare symbol ('claude / 'codex /
;;; 'gemini) — looked up in the table below — or a (<sym> #:package
;;; <pkg>) two-element list for user-supplied packages.  Unrecognised
;;; bare symbols emit a warn-only message at configuration time and
;;; contribute no package.
;;;

(define (gascity-resolve-provider entry)
  "Resolve ENTRY into a package or #f.  The 'claude case is the only
hard-imported package today; 'codex / 'gemini are reserved warn-only and
return #f unless the user supplies (<sym> #:package <pkg>) explicitly."
  (match entry
    ((? symbol? sym)
     (case sym
       ((claude)
        ;; (r0man guix packages claude) is hard-imported lazily so the
        ;; module loads cleanly when the channel is missing claude-code.
        ((@ (r0man guix packages claude) claude-code)))
       ((codex gemini)
        (format (current-error-port)
                "gascity: provider ~a not yet packaged in this channel; \
pass `(~a #:package <pkg>)' to override.~%"
                sym sym)
        #f)
       (else
        (format (current-error-port)
                "gascity: unknown provider ~a; ignoring.~%" sym)
        #f)))
    (((? symbol? _) #:package pkg) pkg)
    (other
     (format (current-error-port)
             "gascity: ignoring unrecognised provider entry ~s~%" other)
     #f)))

(define (gascity-supervisor-provider-packages supervisor)
  "Resolve provider opt-ins for SUPERVISOR, dropping #f entries."
  (filter-map gascity-resolve-provider
              (gascity-supervisor-providers supervisor)))


;;;
;;; Profile package extension.
;;;

(define (home-gascity-profile-packages config)
  "Filter the configured package list: drop dolt and beads-next when no
supervisor uses (beads-provider 'bd); append all provider-resolved packages."
  (let* ((supervisors (home-gascity-supervisors config))
         (any-bd?   (any (lambda (s) (eq? 'bd (gascity-supervisor-beads-provider s)))
                         supervisors))
         (provider-pkgs (append-map gascity-supervisor-provider-packages
                                    supervisors))
         (base (if any-bd?
                   (home-gascity-packages config)
                   (filter (lambda (p)
                             (not (member p (list dolt beads-next))))
                           (home-gascity-packages config)))))
    (append base provider-pkgs)))


;;;
;;; Per-supervisor TOML rendering at host time.
;;;
;;; We pre-render TOML strings as host-side Scheme values and embed
;;; them as literals into the activation gexp.  This keeps the gexp
;;; small and unit-testable independently from the activation machinery.
;;;

(define (city-rig-spec rig)
  "Serialize RIG into an alist consumable by render-rigs-section."
  (let* ((name   (gascity-rig-derived-name rig))
         (prefix (or (gascity-rig-prefix rig)
                     (derive-beads-prefix name))))
    `((path   . ,(gascity-rig-path rig))
      (name   . ,name)
      (prefix . ,prefix))))

(define (city-pack-spec pack)
  `((name   . ,(gascity-pack-name pack))
    (source . ,(gascity-pack-source pack))
    (ref    . ,(gascity-pack-ref pack))
    (path   . ,(gascity-pack-path pack))))

(define (city-agent-spec agent)
  `((name     . ,(gascity-agent-name agent))
    (provider . ,(gascity-agent-provider agent))
    (command  . ,(gascity-agent-command agent))))

(define (city-rendered-toml supervisor city)
  "Render the managed city.toml for CITY belonging to SUPERVISOR.  When the
city has (provider …) set, that string becomes [workspace] provider; the
inherited (beads-provider …) becomes [beads] provider.  Returns a
string."
  (let* ((rigs   (map city-rig-spec  (gascity-city-rigs   city)))
         (packs  (map city-pack-spec (gascity-city-packs  city)))
         (agents (map city-agent-spec (gascity-city-agents city)))
         (provider (gascity-city-provider city))
         (beads (case (gascity-supervisor-beads-provider supervisor)
                  ((bd)   "bd")
                  ((file) "file")
                  (else   ""))))
    (render-city-toml
     #:workspace-name (or (gascity-city-name city)
                          (basename (gascity-city-path city)))
     #:workspace-suspended? (gascity-city-workspace-suspended? city)
     #:provider-default (cond
                         ((string? provider) provider)
                         ((symbol? provider) (symbol->string provider))
                         (else ""))
     #:beads-provider beads
     #:rigs rigs
     #:packs packs
     #:agents agents
     #:providers (gascity-city-providers city)
     #:extra-toml (gascity-city-extra-toml city))))

(define (supervisor-cities-toml supervisor)
  "Render the GC_HOME/cities.toml content listing every city for SUPERVISOR."
  (render-cities-toml
   (map (lambda (city)
          `((path . ,(gascity-city-path city))
            (name . ,(or (gascity-city-name city)
                         (basename (gascity-city-path city))))))
        (gascity-supervisor-cities supervisor))))

(define (supervisor-config-toml supervisor)
  "Render the <gc-home>/supervisor.toml for SUPERVISOR."
  (render-supervisor-toml
   #:port (gascity-supervisor-supervisor-port supervisor)))

(define (supervisor-api-url supervisor)
  "Loopback URL where SUPERVISOR binds its HTTP API."
  (string-append "http://127.0.0.1:"
                 (number->string
                  (gascity-supervisor-supervisor-port supervisor))))

(define (supervisor-city-spec supervisor city)
  "Serialize CITY into a Scheme value usable from the activation gexp.
The rendered managed-mode city.toml string is computed at host time and
inlined."
  (let ((managed? (gascity-city-managed? city)))
    `(("path"      . ,(gascity-city-path city))
      ("managed?"  . ,managed?)
      ("file?"     . ,(eq? 'file (gascity-supervisor-beads-provider supervisor)))
      ("city-toml" . ,(if managed? (city-rendered-toml supervisor city) ""))
      ("supervisor-name"
       . ,(symbol->string (gascity-supervisor-name supervisor))))))

(define (supervisor-spec supervisor)
  "Serialize SUPERVISOR into a flat alist of strings/booleans for the
activation gexp.  All values are quoted into the gexp directly."
  (let ((cities (map (lambda (c) (supervisor-city-spec supervisor c))
                     (gascity-supervisor-cities supervisor))))
    `(("name"            . ,(symbol->string (gascity-supervisor-name supervisor)))
      ("gc-home"         . ,(gascity-supervisor-gc-home supervisor))
      ("cities"          . ,cities)
      ("supervisor-toml" . ,(supervisor-config-toml supervisor))
      ("cities-toml"     . ,(supervisor-cities-toml supervisor))
      ("any-managed?"    . ,(any gascity-city-managed?
                                 (gascity-supervisor-cities supervisor)))
      ("any-file?"       . ,(eq? 'file (gascity-supervisor-beads-provider supervisor))))))


;;;
;;; Activation.
;;;

(define (home-gascity-activation config)
  (let* ((supervisors        (home-gascity-supervisors config))
         (any-bd?            (any (lambda (s)
                                    (eq? 'bd (gascity-supervisor-beads-provider s)))
                                  supervisors))
         (dolt-cfg           (home-gascity-dolt config))
         (dolt-user-name     (gascity-dolt-user-name  dolt-cfg))
         (dolt-user-email    (gascity-dolt-user-email dolt-cfg))
         (gc-bin             (file-append gascity-next "/bin/gc"))
         (specs              (map supervisor-spec supervisors)))
    (with-imported-modules '((guix build utils))
      #~(begin
          (use-modules (guix build utils)
                       (srfi srfi-1)
                       (ice-9 match))
          (let* ((home (getenv "HOME"))
                 (write-atomic
                  (lambda (path content)
                    "Write CONTENT into PATH atomically by writing to
PATH.tmp first, then renaming.  Required so the running supervisor's
patrol-tick os.ReadFile cannot see half-written TOML."
                    (mkdir-p (dirname path))
                    (let ((tmp (string-append path ".tmp")))
                      (call-with-output-file tmp
                        (lambda (port) (display content port)))
                      (rename-file tmp path))))
                 (specs '#$specs))
            ;; (1) Dolt identity seed, once per host.  Skipped when no
            ;; supervisor uses 'bd or when ~/.dolt or ~/.gitconfig already
            ;; exists.  Identity comes from (home-gascity-dolt config).
            #$(if any-bd?
                  #~(let* ((dolt-dir    (string-append home "/.dolt"))
                           (global-json (string-append dolt-dir
                                                       "/config_global.json"))
                           (gitconfig   (string-append home "/.gitconfig")))
                      (unless (or (file-exists? global-json)
                                  (file-exists? gitconfig))
                        (mkdir-p dolt-dir)
                        (call-with-output-file global-json
                          (lambda (out)
                            (display
                             (string-append
                              "{\"user.name\":\"" #$dolt-user-name "\","
                              "\"user.email\":\"" #$dolt-user-email "\"}\n")
                             out)))))
                  #~#t)

            ;; (2) Per-supervisor loop.
            (for-each
             (lambda (spec)
               (let* ((name           (assoc-ref spec "name"))
                      (gc-home-rel    (assoc-ref spec "gc-home"))
                      (gc-home        (string-append home "/" gc-home-rel))
                      (cities         (assoc-ref spec "cities"))
                      (supervisor-toml(assoc-ref spec "supervisor-toml"))
                      (cities-toml    (assoc-ref spec "cities-toml")))
                 (mkdir-p gc-home)
                 ;; supervisor.toml — atomic.
                 (write-atomic (string-append gc-home "/supervisor.toml")
                               supervisor-toml)
                 ;; cities.toml — atomic.
                 (write-atomic (string-append gc-home "/cities.toml")
                               cities-toml)
                 ;; Per-city activation.
                 (for-each
                  (lambda (city)
                    (let* ((path      (assoc-ref city "path"))
                           (managed?  (assoc-ref city "managed?"))
                           (file?     (assoc-ref city "file?"))
                           (city-toml-content (assoc-ref city "city-toml"))
                           (city-toml (string-append path "/city.toml"))
                           (marker    (string-append path "/.guix-home-managed"))
                           (gc-dir    (string-append path "/.gc")))
                      (mkdir-p path)
                      (mkdir-p gc-dir)
                      (cond
                       (managed?
                        ;; Managed city: render city.toml directly.
                        ;; If city.toml exists but the marker is absent,
                        ;; fail loud — we will not clobber a city we did
                        ;; not previously manage.
                        (when (and (file-exists? city-toml)
                                   (not (file-exists? marker)))
                          (error 'home-gascity-activation
                                 "refusing to overwrite unmanaged city.toml; \
remove the file or set (managed? #f)" path))
                        (write-atomic city-toml city-toml-content)
                        (call-with-output-file marker
                          (lambda (port)
                            (display (assoc-ref city "supervisor-name") port)
                            (newline port))))
                       (else
                        ;; Unmanaged city: leave city.toml alone.
                        ;; gascity-init's start thunk will `gc init' it
                        ;; once if it is missing.
                        #t))
                      ;; File-beads bootstrap mirroring upstream's
                      ;; bootstrapScopedFileProviderCityFS: drop the
                      ;; layout marker and an empty beads.json under
                      ;; <city>/.gc/.  Idempotent: skip when the files
                      ;; already exist (matches upstream's stat-and-skip
                      ;; on beads.json).
                      (when file?
                        (let ((layout (string-append gc-dir
                                                     "/file-beads-layout"))
                              (beads-json (string-append gc-dir "/beads.json")))
                          (unless (file-exists? layout)
                            (call-with-output-file layout
                              (lambda (p) (display "scope-local-v1\n" p))))
                          (unless (file-exists? beads-json)
                            (call-with-output-file beads-json
                              (lambda (p)
                                (display "{\"seq\":0,\"beads\":[]}\n" p))))))))
                  cities)))
             specs))))))


;;;
;;; Environment variables.
;;;

(define (home-gascity-environment-variables config)
  "Export GC_HOME pointing at (default-gc-home …) when set, otherwise
at the first supervisor's gc-home.  Multi-supervisor setups can also
`GC_HOME=$HOME/.gc-other gc supervisor status' for ad-hoc reach; see
the module commentary above for why we do not emit multiple GC_HOME
values."
  (let* ((supervisors (home-gascity-supervisors config))
         (explicit    (home-gascity-default-gc-home config))
         (gc-home     (or explicit
                          (gascity-supervisor-gc-home (first supervisors)))))
    `(("GC_HOME" . ,(string-append "$HOME/" gc-home)))))


;;;
;;; Shell completions.
;;;
;;; `gc completion <shell>' is a stock cobra command — no network, no
;;; runtime config — so it runs cleanly inside the Guix build sandbox.
;;; Generated at BUILD time, not activation, so failures are isolated to
;;; `guix build' and the artifact is reproducible.  The resulting script
;;; embeds the build-time absolute path of `gc' in the store, exactly
;;; what a Guix-managed completion wants.
;;;

(define (gc-completion-file shell)
  "Build a computed-file containing the cobra completion script for
SHELL (a string: \"bash\", \"zsh\", or \"fish\").  Read via get-string-all
from (ice-9 textual-ports) — `dump-port' is not stdlib in current Guile
and would fail at build time."
  (computed-file
   (string-append "gc-" shell "-completion")
   #~(begin
       (use-modules (ice-9 popen) (ice-9 textual-ports))
       (let ((p (open-pipe* OPEN_READ
                            #$(file-append gascity-next "/bin/gc")
                            "completion" #$shell)))
         (call-with-output-file #$output
           (lambda (out) (display (get-string-all p) out)))
         (close-pipe p)))))

(define gc-bash-completion (gc-completion-file "bash"))
(define gc-zsh-completion  (gc-completion-file "zsh"))
(define gc-fish-completion (gc-completion-file "fish"))

(define (home-gascity-bash-extension config)
  "Return a home-bash-extension that sources the gc bash completion
script.  home-bash-extension renders entries in (bashrc …) as `source
<path>' lines in ~/.bashrc — the script body itself does not appear
inline in .bashrc."
  (home-bash-extension
   (bashrc (list gc-bash-completion))))

(define (home-gascity-files config)
  "Return zsh/fish completion scripts for users running shells other
than bash.  ~/.config/gascity/completions/{gc.zsh,gc.fish} can be
sourced from the user's own zshrc / config.fish."
  `((".config/gascity/completions/gc.zsh"  ,gc-zsh-completion)
    (".config/gascity/completions/gc.fish" ,gc-fish-completion)))


;;;
;;; Shepherd services per supervisor.
;;;

(define (supervisor-shepherd-services supervisor)
  "Return shepherd services for SUPERVISOR: long-running
gascity-supervisor-<NAME>, one-shot gascity-init-<NAME>, and (when
dashboard? is true) long-running gascity-dashboard-<NAME>."
  (let* ((sup-name   (gascity-supervisor-provision-symbol
                      'gascity-supervisor supervisor))
         (init-name  (gascity-supervisor-provision-symbol
                      'gascity-init supervisor))
         (dash-name  (gascity-supervisor-provision-symbol
                      'gascity-dashboard supervisor))
         (gc-home    (gascity-supervisor-gc-home supervisor))
         (cities     (gascity-supervisor-cities supervisor))
         (city-specs (map (lambda (city)
                            (list (gascity-city-path city)
                                  (or (gascity-city-name city)
                                      (basename (gascity-city-path city)))
                                  (gascity-city-managed? city)
                                  (map (lambda (rig)
                                         (list (gascity-rig-path rig)
                                               (or (gascity-rig-git-url rig) "")
                                               (or (gascity-rig-branch rig) "")
                                               (or (gascity-rig-depth rig) 1)))
                                       (gascity-city-rigs city))))
                          cities))
         ;; (city-path . template-store-path) for cities with
         ;; (init-from …) set.  Built as a gexp list — NOT folded into
         ;; city-specs — so the file-like template sources are lowered
         ;; via #$ rather than embedded as record literals by the
         ;; '#$city-specs quoting.
         (init-from-alist
          #~(list #$@(filter-map
                      (lambda (c)
                        (and (gascity-city-init-from c)
                             #~(cons #$(gascity-city-path c)
                                     #$(gascity-city-init-from c))))
                      cities)))
         (gc-bin     (file-append gascity-next "/bin/gc"))
         (git-bin    (file-append git "/bin/git"))
         (timeout-bin(file-append coreutils "/bin/timeout"))
         (chmod-bin  (file-append coreutils "/bin/chmod"))
         (find-bin   (file-append findutils "/bin/find"))
         (dashboard-port (gascity-supervisor-dashboard-port supervisor)))
    (define (with-env log-name body)
      "Wrap BODY in a start thunk that binds names HOME, PROFILE,
GC-HOME-ABS, ENV and LOG-FILE.  LOG-NAME is the basename of the
per-service log file under GC_HOME."
      #~(lambda _
          (let* ((home user-homedir)
                 (profile (string-append home "/.guix-home/profile"))
                 (gc-home-abs (string-append home "/" #$gc-home))
                 (xdg-runtime (or (getenv "XDG_RUNTIME_DIR")
                                  (string-append "/run/user/"
                                                 (number->string (getuid)))))
                 (env (cons*
                       (string-append "GC_HOME=" gc-home-abs)
                       (string-append "PATH=" profile "/bin:" profile "/sbin")
                       (string-append "XDG_RUNTIME_DIR=" xdg-runtime)
                       (string-append "SSL_CERT_DIR=" profile "/etc/ssl/certs")
                       (string-append "SSL_CERT_FILE=" profile
                                      "/etc/ssl/certs/ca-certificates.crt")
                       (string-append "GIT_SSL_CAINFO=" profile
                                      "/etc/ssl/certs/ca-certificates.crt")
                       (user-environment-variables)))
                 (log-file (string-append gc-home-abs "/" #$log-name)))
            #$body)))
    (let ((services
           (list
            (shepherd-service
             (documentation
              (string-append "Run 'gc supervisor run' for supervisor "
                             (symbol->string (gascity-supervisor-name supervisor))
                             "."))
             (provision (list sup-name))
             (modules '((shepherd support)
                        (guix build utils)))
             (respawn? #t)
             (respawn-limit #~(cons 3 30))
             (start
              (with-env "supervisor.log"
                #~(begin
                    (mkdir-p gc-home-abs)
                    (fork+exec-command
                     (list #$gc-bin "supervisor" "run")
                     #:directory home
                     #:log-file log-file
                     #:environment-variables env))))
             (stop #~(make-kill-destructor)))
            (shepherd-service
             (documentation
              (string-append "Lay out cities and rigs for supervisor "
                             (symbol->string (gascity-supervisor-name supervisor))
                             "; reload the supervisor."))
             (provision (list init-name))
             (one-shot? #t)
             (requirement (list sup-name))
             (modules '((shepherd support)
                        (ice-9 textual-ports)
                        (guix build utils)))
             (start
              (with-env "init.log"
                #~(let ((run (lambda (dir argv)
                               (waitpid (fork+exec-command
                                         argv
                                         #:directory dir
                                         #:log-file log-file
                                         #:environment-variables env)))))
                    ;; Wait up to 30 s for the supervisor to be reachable.
                    (let loop ((tries 60))
                      (let ((status (cdr (run home (list #$gc-bin "supervisor"
                                                         "status")))))
                        (cond ((zero? (status:exit-val status)) #t)
                              ((zero? tries)
                               (format (current-error-port)
                                       "gascity-init: 'gc supervisor status' \
did not report running within 30s; proceeding anyway~%"))
                              (else (usleep 500000) (loop (- tries 1))))))
                    (for-each
                     (lambda (city-spec)
                       (let* ((city-path (list-ref city-spec 0))
                              (managed?  (list-ref city-spec 2))
                              (rigs      (list-ref city-spec 3))
                              (city-toml (string-append city-path
                                                        "/city.toml")))
                         (mkdir-p city-path)
                         ;; One-time bootstrap when city.toml is absent:
                         ;;  - (init-from …) set → deep-copy the
                         ;;    template via `gc init --from <src>'
                         ;;    (validation guarantees managed? is #f);
                         ;;  - else unmanaged → bare `gc init'.
                         ;; Managed cities get their city.toml from the
                         ;; activation render, so no `gc init' at all.
                         (let ((src (assoc-ref #$init-from-alist
                                                city-path)))
                           (cond
                            ((and src (not (file-exists? city-toml)))
                             ;; Deep-copy the template verbatim — `gc
                             ;; init --from' must point straight at the
                             ;; store path (a renamed staging copy makes
                             ;; gc scaffold a minimal city instead).
                             (run home (list #$gc-bin "init" "--from"
                                             src city-path))
                             ;; The template lives in the read-only
                             ;; store (0444), so the copied
                             ;; city.toml/pack.toml/packs/* land
                             ;; read-only and a (managed? #f) city is
                             ;; not runtime-mutable.  Add owner-write
                             ;; back, but `-prune' gc's freshly created
                             ;; .beads/.gc dolt runtime: a recursive
                             ;; chmod over it races bd's schema
                             ;; migration (duplicate-key on `wisps').
                             ;; find never descends the pruned subtrees,
                             ;; so the live dolt DB is never touched.
                             (run home
                                  (list #$find-bin city-path
                                        "-name" ".beads" "-prune" "-o"
                                        "-name" ".gc" "-prune" "-o"
                                        "(" "-type" "f" "-o"
                                            "-type" "d" ")"
                                        "-exec" #$chmod-bin "u+w"
                                        "{}" "+")))
                            ((and (not managed?)
                                  (not (file-exists? city-toml)))
                             (run home (list #$gc-bin "init" city-path)))
                            (else #t)))
                         (for-each
                          (lambda (rig-spec)
                            (let* ((rig-path (list-ref rig-spec 0))
                                   (git-url  (list-ref rig-spec 1))
                                   (branch   (list-ref rig-spec 2))
                                   (depth    (list-ref rig-spec 3))
                                   (abs-rig  (if (string-prefix? "/" rig-path)
                                                 rig-path
                                                 (string-append city-path "/"
                                                                rig-path))))
                              (when (and (not (file-exists? abs-rig))
                                         (not (string-null? git-url)))
                                (let* ((argv (cons*
                                              #$git-bin "clone"
                                              (append
                                               (if (string-null? branch)
                                                   '()
                                                   (list "--branch" branch))
                                               (if (and depth
                                                        (number? depth)
                                                        (positive? depth))
                                                   (list "--depth"
                                                         (number->string depth))
                                                   '())
                                               (list git-url abs-rig)))))
                                  (run home argv)))
                              ;; `gc rig add' is idempotent only via the
                              ;; reAdd fast-path (cmd_rig.go:243-268),
                              ;; which requires the rig name to already be
                              ;; in city.toml.  When city.toml lacks the
                              ;; entry but <rig>/.beads is populated
                              ;; (unmanaged cities, partial first runs,
                              ;; out-of-band `bd init'), the fresh-add
                              ;; guard (cmd_rig.go:308-321) errors with
                              ;; "use --adopt".  Pass --adopt only when
                              ;; BOTH .beads/metadata.json AND
                              ;; .beads/config.yaml exist — --adopt
                              ;; requires both (cmd_rig.go:357-366 reads
                              ;; metadata.json and calls readBeadsPrefix
                              ;; on config.yaml; a missing or empty
                              ;; issue_prefix errors out).  A partial bd
                              ;; init leaving only metadata.json would
                              ;; otherwise trip --adopt and fail.  The
                              ;; residual case (config.yaml present but
                              ;; malformed) falls through to upstream's
                              ;; error message.
                              (let* ((meta (string-append abs-rig
                                                          "/.beads/metadata.json"))
                                     (config (string-append abs-rig
                                                            "/.beads/config.yaml"))
                                     (argv (if (and (file-exists? meta)
                                                    (file-exists? config))
                                               (list #$gc-bin "rig" "add"
                                                     "--adopt" abs-rig)
                                               (list #$gc-bin "rig" "add"
                                                     abs-rig))))
                                (run city-path argv))))
                          rigs)))
                     '#$city-specs)
                    ;; Trigger a reload so the supervisor re-reads
                    ;; cities.toml without waiting for the patrol tick.
                    ;; `gc supervisor reload' itself blocks up to 5 min
                    ;; (supervisorReloadWaitTimeout, cmd_supervisor.go:287;
                    ;; reload-socket handler at :381-394); cap with
                    ;; `timeout 30 …'.  run returns (pid . status) but
                    ;; its value is discarded — the trailing #t at the
                    ;; end of this start thunk is what shepherd sees, so
                    ;; a timeout(1) exit (124) does not fail this
                    ;; one-shot.  The supervisor's 10-s patrol tick
                    ;; surfaces the new state regardless.
                    (run home (list #$timeout-bin "30"
                                    #$gc-bin "supervisor" "reload"))
                    #t))))
            (and (gascity-supervisor-dashboard? supervisor)
                 (shepherd-service
                  (documentation
                   (string-append
                    "Run 'gc dashboard serve' for supervisor "
                    (symbol->string (gascity-supervisor-name supervisor))
                    "."))
                  (provision (list dash-name))
                  (requirement (list sup-name))
                  (modules '((shepherd support)
                             (guix build utils)))
                  (respawn? #t)
                  (respawn-limit #~(cons 3 30))
                  (start
                   (with-env "dashboard.log"
                     #~(begin
                         (mkdir-p gc-home-abs)
                         ;; Pass --api explicitly: shepherd's requirement
                         ;; only waits for `gc supervisor run' to fork —
                         ;; the HTTP port may not be bound yet, and
                         ;; `gc dashboard serve' bails out (and is
                         ;; respawned) when auto-discovery sees the
                         ;; supervisor PID file but no live socket.
                         (fork+exec-command
                          (list #$gc-bin "dashboard" "serve"
                                "--port" #$(number->string dashboard-port)
                                "--api" #$(supervisor-api-url supervisor))
                          #:directory home
                          #:log-file log-file
                          #:environment-variables env))))
                  (stop #~(make-kill-destructor)))))))
      (filter shepherd-service? services))))

(define (home-gascity-shepherd-services config)
  (home-gascity-validate-configuration! config)
  (append-map supervisor-shepherd-services
              (home-gascity-supervisors config)))


;;;
;;; Service-type definition.
;;;

(define home-gascity-service-type
  (service-type
   (name 'home-gascity)
   (extensions
    (list (service-extension home-profile-service-type
                             home-gascity-profile-packages)
          (service-extension home-shepherd-service-type
                             home-gascity-shepherd-services)
          (service-extension home-environment-variables-service-type
                             home-gascity-environment-variables)
          (service-extension home-activation-service-type
                             home-gascity-activation)
          (service-extension home-bash-service-type
                             home-gascity-bash-extension)
          (service-extension home-files-service-type
                             home-gascity-files)))
   (description
    "Multi-supervisor Gas City home service.  Each
<gascity-supervisor-configuration> in (supervisors …) gets its own
GC_HOME, a gascity-supervisor-<NAME> shepherd service running 'gc
supervisor run', and a gascity-init-<NAME> one-shot that lays out cities,
clones rigs, and reloads the supervisor.  Host-level dolt identity comes
from the top-level (dolt …) <gascity-dolt-configuration> record;
activation seeds ~/.dolt/config_global.json once per host (only when at
least one supervisor uses (beads-provider 'bd) and ~/.gitconfig is also
missing).")))

;;; gascity.scm ends here
