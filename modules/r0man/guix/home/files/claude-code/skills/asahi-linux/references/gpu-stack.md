# GPU stack: kernel `drm/asahi` + Mesa `asahi` / `honeykrisp`

Apple's GPU (AGX) is proprietary, undocumented, and substantially different from any PC GPU. The Asahi GPU stack is one of the most significant clean-room driver projects in recent Linux history.

## Architecture

```
User application (OpenGL / Vulkan / OpenCL)
  └─▶ Mesa (upstream)
        ├─▶ asahi       — OpenGL 4.6 + OpenGL ES 3.2 driver
        └─▶ honeykrisp  — Vulkan 1.3 driver
              │
              └─▶ UAPI (DRM/KMS)
                    │
                    └─▶ drm/asahi (in-kernel, written in Rust)
                          │
                          └─▶ AGX firmware (blob loaded from macOS)
                                │
                                └─▶ GPU hardware
```

Two things make this stack unusual:

1. **`drm/asahi` is written in Rust** — one of the largest Rust drivers in the Linux tree (around 21k lines). Asahi Lina is the primary author.
2. **The GPU "firmware" is an entire OS running on the GPU**. Apple's firmware runs a complete RTOS on the GPU itself and exposes a high-level command API. The Rust driver's main job is to marshal commands, not to drive the hardware at low level.

## Where the code lives

### Kernel

- Upstream-bound: in the `linux` repo at `drivers/gpu/drm/asahi/`.
- Active development branch: `asahi` on https://github.com/AsahiLinux/linux (currently the user has `asahi-6.13.8` checked out locally at `~/workspace/linux`).
- Feature branches to know:
  - `asahi-wip` — latest unstable WIP.
  - `fairydust` — DisplayPort alt-mode (USB-C video output).
  - Release branches track mainline: `asahi-6.19`, `asahi-6.18`, etc.

Upstreaming status: M1/M2 support is almost entirely mainline as of 6.19. The fork carries WIP driver work (cleanups for upstream submission) and newer hardware (M3, M4) that hasn't fully landed yet.

### Userspace (Mesa)

- **Fully upstream** in https://gitlab.freedesktop.org/mesa/mesa.
- `src/asahi/` — OpenGL driver.
- `src/asahi/vulkan/` — `honeykrisp` Vulkan driver.
- **There is no `AsahiLinux/mesa` repo** — the fork was upstreamed and deleted. Tutorials pointing there are obsolete.
- Fedora Asahi Remix still maintains a downstream Mesa package (https://pagure.io/fedora-asahi/mesa) for Remix packaging, but the driver code itself is all upstream.

## What works

Check the live Feature Support matrix — https://asahilinux.org/docs/platform/feature-support/overview/ — for the definitive answer. High-level summary as of kernel 6.19:

- **OpenGL 4.6** + OpenGL ES 3.2 on M1/M2 family.
- **Vulkan 1.3** with enough extensions for DXVK, vkd3d-proton, and many native Vulkan apps. Some advanced extensions are still missing — check Mesa release notes for specifics.
- **Headless compute** via OpenCL (in Mesa's Rusticl; maturing).
- **M3 GPU**: WIP — basic support may work but expect issues.
- **M4**: WIP.

## Gaming performance

- OpenGL is often **faster** than Vulkan for DX8-DX11 titles under Proton because the Asahi GL driver has optimizations that haven't landed on Vulkan yet. See `x86-emulation.md` for the `PROTON_USE_WINED3D=1` trick.
- TSO mode is enabled automatically inside muvm — memory ordering is not a bottleneck.
- Page-size overhead: x86 games always run in the 4 KB VM (muvm) — this is fine for perf.

## Display pipeline (`appledrm` / Apple DCP)

Separate from the GPU driver: the display controller driver is `appledrm` (previously `apple_dcp`). Provides:

- Internal MacBook/iMac display at native refresh and resolution.
- Notch cutout.
- Color management.
- `modetest` / DRM-standard interfaces.

The `appledrm.show_notch=1` kernel parameter extends the display area **above** the notch — apps can then draw into the corners.

External display:
- **HDMI** on desktops (Mac mini, Mac Studio) works.
- **DisplayPort over USB-C alt-mode** is the `fairydust` branch of the kernel — not yet upstream or in stable Fedora. Progress is in the quarterly reports.

## Kernel development workflow

If the user wants to hack on the GPU driver:

```sh
git clone https://github.com/AsahiLinux/linux.git
cd linux
git checkout asahi-wip                  # or asahi-<version> for a released branch

# For GPU work
cd drivers/gpu/drm/asahi/
# Code is pure Rust. Kernel Rust requires nightly-ish toolchain config —
# see Documentation/rust/ in the kernel tree.
```

Building the kernel: `make LLVM=1 -j$(nproc)` — Rust in the kernel requires `clang`/`llvm` toolchains. Fedora ships working LLVM; on other distros install the matching LLVM version listed in `Documentation/process/changes.rst`.

For Mesa work:

```sh
git clone https://gitlab.freedesktop.org/mesa/mesa.git
cd mesa
# Build the asahi driver specifically
meson setup build -Dgallium-drivers=asahi -Dvulkan-drivers=asahi
meson compile -C build
```

## Reverse engineering the GPU

The original AGX work was done by **Alyssa Rosenzweig** using macOS driver traces. New work (new SoCs, new hardware features) typically involves:

1. Running macOS under **`m1n1`'s hypervisor mode** (`m1n1/proxyclient/hv.py`) to intercept GPU driver calls.
2. Comparing firmware command stream captures across macOS versions.
3. Analyzing Apple's driver binary with Ghidra / IDA.

See `upstream-dev.md` for the m1n1 hypervisor workflow.

## Common GPU issues

**"Games run but performance is bad"** — known tradeoffs documented in `x86-emulation.md`. Try WineD3D for DX8-DX11.

**"Vulkan app says extension X missing"** — check the Mesa release notes; `honeykrisp` is a rapidly-moving target.

**"Screen is black after wake from sleep"** — the display pipeline has some suspend/resume issues on certain M2 configs. Check the Fedora Asahi Remix bug tracker; a fresh kernel often fixes it.

**"External display doesn't work over USB-C"** — DisplayPort alt-mode is on the `fairydust` branch and not stable. Check the latest progress report.

**"GPU hangs after heavy load"** — often a firmware issue. Confirm firmware is up to date (`sudo asahi-fwupdate`), and include the exact `dmesg` GPU crash log when reporting upstream.

## Honeykrisp naming

`honeykrisp` is the Vulkan driver (named after the apple cultivar). If a user or forum post mentions "honeykrisp doesn't work with X game," this is a Vulkan-specific issue; GL under the same scenario may be fine.
