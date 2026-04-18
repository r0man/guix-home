# Canonical resources

The single most important thing to know: where to find current, authoritative answers when the skill is out of date.

## Documentation

| Source | URL | Scope |
|---|---|---|
| **Asahi Linux docs (official)** | https://asahilinux.org/docs/ | Upstream project, feature support matrix, platform details |
| **Fedora Asahi Remix docs** | https://docs.fedoraproject.org/en-US/fedora-asahi-remix/ | The reference distro (install, troubleshooting, x86 apps) |
| **Feature Support matrix** | https://asahilinux.org/docs/platform/feature-support/overview/ | What works on which hardware — the source of truth |
| **Progress reports** | https://asahilinux.org/blog/ | Quarterly state of the project |
| **Introduction to Apple Silicon** | https://asahilinux.org/docs/platform/introduction/ | Platform background |
| **Open OS interop on Apple Silicon** | https://asahilinux.org/docs/platform/open-os-interop/ | Boot flow, firmware provisioning, partition layout |

The Fedora Asahi Remix docs source is at https://pagure.io/fedora-asahi/docs-site (Antora / AsciiDoc). User has a local clone at `~/workspace/docs-site/`.

## GitHub: https://github.com/AsahiLinux

Key repos (see `upstream-dev.md` for the full map and per-subsystem pointers):

- `m1n1` — bootloader + hypervisor
- `linux` — kernel tree (branches: `asahi-wip`, `asahi-<version>`, `fairydust`)
- `asahi-installer` — macOS-side installer
- `asahi-scripts` — post-install userspace helpers (includes `asahi-diagnose`)
- `speakersafetyd` — speaker protection (Rust)
- `tiny-dfr` — Touch Bar daemon (Rust)
- `muvm` — microVM runner for 4K page / x86 emulation
- `asahi-audio` — DSP configs
- `docs` — asahilinux.org/docs (MkDocs)
- `PKGBUILDs` — Arch packaging (historical)

Mesa GPU drivers are **upstream**: https://gitlab.freedesktop.org/mesa/mesa (paths `src/asahi/` + `src/asahi/vulkan/`). No `AsahiLinux/mesa` repo exists.

## Fedora Asahi SIG

- FAS group: `asahi-sig` — https://src.fedoraproject.org/group/asahi-sig
- COPR: https://copr.fedorainfracloud.org/groups/g/asahi/coprs/ (packages that can't live in Fedora main)
- Pagure projects: https://pagure.io/projects/fedora-asahi/* (installer, Kiwi descriptions, website, branding)
- Kernel fork: https://gitlab.com/fedora-asahi/kernel-asahi (fork of kernel-ark + upstream Asahi patches)
- Bug tracker: https://pagure.io/fedora-asahi/remix-bugs/issues

## Community

- **Fedora forum**: https://discussion.fedoraproject.org/c/neighbors/asahi/92 — the canonical user support / triage venue (replaces the older Discourse).
- **IRC (OFTC)**: `#asahi` (users), `#asahi-dev` (kernel/userspace), `#asahi-gpu` (driver), `#asahi-re` (reverse engineering), `#asahi-alt` (alt distros).
- **Mastodon**: `@AsahiLinux@treehouse.systems`.

## Install URLs

- https://fedora-asahi-remix.org/install — Fedora-branded installer, stable.
- https://alx.sh / https://asahilinux.org/install — upstream Asahi branding, more options (including non-Fedora targets).
- https://fedora-asahi-remix.org/builds — beta and nightly Fedora Asahi Remix.

Both point at the same binary payload; differences are CDN provider and logo. See `docs-site/faq.adoc` for the full rationale.

## Non-Fedora distros

Fedora Asahi Remix is the only distribution officially endorsed by the Asahi project today. Other setups exist and are community-maintained:

- **Arch Linux ARM** — https://github.com/asahi-alarm/PKGBUILDs (community org) and https://github.com/AsahiLinux/PKGBUILDs (older, partially active).
- **NixOS Apple Silicon** — https://github.com/tpwrules/nixos-apple-silicon — actively maintained. Ports most userspace bits as `nixpkgs` packages (`asahi-audio`, `asahi-bless`, `asahi-fwextract`, `asahi-nvram`, `asahi-wifisync`).
- **Asahi Ubuntu / Debian** — unofficial attempts exist; quality varies widely.
- **Asahi Guix** — the user's own project; covered in the `asahi-guix` skill, not here.

## Repo inventory the user has checked out locally

If the user references something from their `~/workspace/`, these are the local clones available for reading without network:

| Path | What it is |
|---|---|
| `~/workspace/docs-site` | Fedora Asahi Remix docs (Antora source) |
| `~/workspace/asahi-installer` | Upstream installer |
| `~/workspace/asahi-scripts` | Upstream post-install scripts (includes `asahi-diagnose`) |
| `~/workspace/fedora-asahi-remix-scripts` | Fedora-specific post-install (branding, SELinux, swap) |
| `~/workspace/linux` | Upstream Asahi kernel tree (user is on `asahi-6.13.8`) |
| `~/workspace/mesa` | Fedora COPR dist-git for Mesa (spec + patches) — not the driver source |
| `~/workspace/speakersafetyd` | Upstream speaker protection daemon |
| `~/workspace/FEX` | FEX-Emu upstream (note: lives at github.com/FEX-Emu, not AsahiLinux org) |
| `~/workspace/PKGBUILDs` | `AsahiLinux/PKGBUILDs` — Arch packaging |
| `~/workspace/nixos-apple-silicon` | Third-party NixOS config |
| `~/workspace/nixpkgs` | nixpkgs (has asahi-* packages under `pkgs/by-name/as/`) |
| `~/workspace/m1n1.backup` | **Older frozen snapshot** — not current, do not treat as source of truth |

For the latest Mesa driver source, fetch from gitlab.freedesktop.org — the Fedora mesa repo has only packaging.

## When the skill is probably wrong

Rate-limit your trust in this skill when:

- The question involves M3 or M4 hardware (the matrix and latest Progress Report are more reliable).
- The answer hinges on what's landed in mainline kernel in the last ~3 months.
- The user is on a non-Fedora distro — package names, paths, and systemd unit names may differ from what's documented here.

Default to citing https://asahilinux.org/docs/ or the latest progress report rather than giving a definitive answer from memory when the question touches a fast-moving area.
