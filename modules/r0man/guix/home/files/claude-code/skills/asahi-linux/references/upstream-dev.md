# Contributing to upstream Asahi Linux

Help for hacking on the Asahi stack itself — the bootloader, kernel drivers, userspace daemons. This is distinct from "using Asahi"; if the user's goal is just running Fedora Asahi Remix, route them to the other references.

## First: read the latest Progress Report

https://asahilinux.org/blog/ — quarterly (sometimes more frequent) reports from marcan, Alyssa, Asahi Lina, and the rest of the team. These describe **exactly** what landed, what's broken, and what's ready for contribution. Before pitching work or answering a contribution question, check the most recent report. A response that contradicts it is almost certainly wrong.

URL pattern: `https://asahilinux.org/<YYYY>/<MM>/<slug>/`. Recent:
- Linux 6.19
- Linux 6.18
- Linux 6.17

## Where everything lives

### GitHub org: https://github.com/AsahiLinux

| Subsystem | Primary repo | Notes |
|---|---|---|
| Stage-1/2 bootloader + hypervisor | `m1n1` | Python + C. Also the RE playground. |
| Kernel tree | `linux` | branch `asahi-wip` for WIP, `asahi-<ver>` per release |
| Installer (macOS-side) | `asahi-installer` | Python |
| Post-install scripts | `asahi-scripts` | `asahi-diagnose`, `update-m1n1`, dracut modules, macsmc-battery |
| Speaker safety | `speakersafetyd` | Rust; critical for speaker health |
| Touch Bar daemon | `tiny-dfr` | Rust |
| NVRAM tooling | `asahi-nvram` | Rust; boot disk selection, BT/WiFi sync experiments |
| USB-C DFU tooling | `macvdmtool`, `tuxvdmtool`, `vdmtool` | Per-OS VDM tools (mac/Linux/Arduino) |
| VM runner for 4K / x86 | `muvm` | Rust; on libkrun |
| binfmt dispatcher | `binfmt-dispatcher` | Rust |
| FEX rootfs generator | `fex-emu-rootfs-generator` | Rust; systemd generator |
| Audio profiles | `asahi-audio` | PipeWire/LSP DSP chains per machine |
| ALSA UCM | `alsa-ucm-conf-asahi` | per-SoC audio routing overlays |
| Docs site | `docs` | MkDocs; rendered at asahilinux.org/docs |

### Mesa (GPU userspace)

Upstream only: https://gitlab.freedesktop.org/mesa/mesa — directories `src/asahi/` (GL) and `src/asahi/vulkan/` (`honeykrisp`).

**There is no `AsahiLinux/mesa` repo.** Old tutorials that reference one are stale.

### Fedora Asahi packaging

- https://src.fedoraproject.org/group/asahi-sig — Fedora-proper packages.
- https://pagure.io/projects/fedora-asahi/* — Pagure projects (Kiwi descriptions, downstream mesa, downstream u-boot, website, etc.).
- https://gitlab.com/fedora/sigs/asahi/* — installer infrastructure (AWS Lambdas, manifest generator).
- https://copr.fedorainfracloud.org/groups/g/asahi/coprs/ — COPR group for packages that can't live in Fedora main.

### Arch Linux ARM packaging

- https://github.com/asahi-alarm/PKGBUILDs — community-maintained. Officially outside the AsahiLinux org now.
- https://github.com/AsahiLinux/PKGBUILDs also exists (historical + some packaging work still done there).

## m1n1: the Swiss army knife

`m1n1` does three jobs:
1. Stage-1 bootloader (in EFI, rarely updated).
2. Stage-2 bootloader (loads U-Boot).
3. **Hypervisor + proxy** — the reverse-engineering Swiss army knife.

### Hypervisor mode — how new hardware gets supported

When Apple ships a new SoC or hardware block, the typical RE workflow is:

1. Boot macOS with the relevant driver loaded.
2. Capture register accesses and firmware command streams via `m1n1`'s hypervisor (`m1n1/proxyclient/hv.py`).
3. Diff traces against older working SoCs.
4. Reverse-engineer command semantics.
5. Implement in Linux.

Script entry point: `m1n1/proxyclient/hv.py` drives `m1n1` over USB/serial while macOS boots. `hv/` subdir has tracepoints. See the m1n1 README for the full proxy-client workflow.

### Updating stage 1 safely

Users don't update stage 1 often. If you're doing kernel work that requires it, expect to re-run the installer's "Repair" (`p`) or "Upgrade m1n1" (`m`) option from recoveryOS. Stage 1 updates touch the Boot Policy, which is nvram-persistent; do not implement a "just swap the stage 1 binary" approach.

## Kernel development

### Getting the tree

```sh
git clone https://github.com/AsahiLinux/linux.git
cd linux
git checkout asahi-wip       # latest unstable
# or git checkout asahi-6.19  # tied to a specific upstream release
```

### Building

Kernel requires Rust (for `drm/asahi`) and LLVM:

```sh
make LLVM=1 rustavailable             # check toolchain
make LLVM=1 ARCH=arm64 defconfig
make LLVM=1 ARCH=arm64 -j$(nproc)
```

See `Documentation/rust/` and `Documentation/process/changes.rst` in the tree for exact toolchain versions. The **Rust toolchain version matters** — the kernel pins a specific `rustc` version; Fedora's system toolchain may or may not match.

### Device trees

Apple devices each need a DTS. Location in tree:

```
arch/arm64/boot/dts/apple/
```

Files are named `t<soc>-<machine>.dts` — e.g., `t8103-j293.dts` for M1 13" MacBook Pro. Each adds per-machine tweaks (audio DSP configs, display nodes, power configuration).

Adding support for a new machine often starts here, especially for audio/speaker differences within the same SoC family.

### Subsystems most actively worked on

- `drivers/gpu/drm/asahi/` — GPU driver (Rust), Alyssa / Asahi Lina.
- `drivers/soc/apple/` — SoC-level platform drivers.
- `drivers/iommu/apple-dart.c` — Apple's IOMMU.
- `drivers/nvme/host/apple.c` — internal NVMe.
- `drivers/pinctrl/pinctrl-apple-gpio.c` — GPIO.
- `drivers/firmware/apple/` — firmware services (AOP, SMC, ISP).

### Upstreaming workflow

Patches go through the normal kernel process: post to the appropriate list (get-maintainer.pl), iterate on review, land via the subsystem tree. For driver-specific cycles see the latest progress report — sometimes there are coordination channels.

## Rust userspace: the "good first contribution" surface

Smaller, self-contained, Rust projects where first-time contributors often land:

- `speakersafetyd` — speaker profile tuning for new machines, new DSP features.
- `tiny-dfr` — Touch Bar layout configs, new icons.
- `asahi-nvram` — new NVRAM variable handling, GUI tooling.
- `muvm` — VM lifecycle improvements, perf, new passthrough protocols.
- `asahi-installer` — UX improvements, new installer menu options.

Each has standard Rust project setup (`cargo build`, unit tests where relevant).

## Communication

- **IRC**: `#asahi`, `#asahi-dev`, `#asahi-gpu`, `#asahi-re`, `#asahi-alt` on OFTC (`irc.oftc.net`). Matrix bridge also available.
- **Mastodon**: `@AsahiLinux@treehouse.systems`.
- **Fedora forum**: https://discussion.fedoraproject.org/c/neighbors/asahi/92 — Fedora Asahi Remix triage, distro discussion.
- **Bug trackers**:
  - Upstream Asahi: https://github.com/AsahiLinux/*/issues (per-repo).
  - Fedora Asahi Remix: https://pagure.io/fedora-asahi/remix-bugs/issues.

## Licensing

- Most kernel code: GPL-2.0.
- m1n1: MIT.
- Mesa drivers: MIT (upstream Mesa convention).
- Rust userspace daemons: MIT or similar permissive.

If the user is integrating Asahi work into another project, check the specific repo's LICENSE — it's not uniformly GPL.

## Useful one-liners for navigating the stack

```sh
# Find your Apple SoC identifier (t<hex>)
cat /proc/device-tree/compatible

# Find your machine model (for DTS lookup)
cat /proc/device-tree/model

# Which m1n1 stage 2 is installed?
cat /boot/efi/m1n1/boot.bin | strings | grep -i version | head -3

# Which kernel branch you're on (check mainlineness)
uname -r

# What GPU driver does Mesa see?
glxinfo | grep -i renderer         # GL
vulkaninfo | grep -i deviceName    # Vulkan
```
