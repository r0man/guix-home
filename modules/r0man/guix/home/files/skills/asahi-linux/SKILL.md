---
name: asahi-linux
description: Expert knowledge for Asahi Linux — running, troubleshooting, and hacking on Linux for Apple Silicon Macs (M1/M2/M3/M4). Use this skill whenever the user mentions Apple Silicon Linux, Asahi, Fedora Asahi Remix, m1n1, alx.sh, or asks about bootloaders, kernel/GPU/audio/firmware, x86 emulation with muvm/FEX, DFU recovery, or Steam/gaming on M-series Macs — even if they describe the hardware as "M1 MacBook" or "Apple Silicon" without naming Asahi. Also use for contributing upstream (m1n1 hypervisor, kernel drivers, speakersafetyd, Mesa asahi/honeykrisp).
---

# Asahi Linux Expert

Asahi Linux is the effort to bring Linux to Apple Silicon Macs. This skill covers both sides:

- **Running Asahi** — troubleshooting, installation, audio, graphics, firmware, x86 games, recovery.
- **Hacking on the upstream stack** — m1n1 bootloader/hypervisor, the kernel tree, Mesa drivers, Rust userspace tools.

The reference distribution is **Fedora Asahi Remix**. Most user-facing advice below targets it unless noted, because the upstream packaging story (repos, installer, systemd units) is built around Fedora. Arch Linux ARM and NixOS setups exist but are community-maintained.

**Not covered here:** Guix packaging of Asahi bits. That's a separate skill (`asahi-guix`).

## ⚠️ Critical safety facts — read before any advice

These are the things that have bitten real users and that a skill should always surface before issuing commands:

1. **`speakersafetyd` is mandatory, not optional.** Apple Silicon speakers have no hardware overcurrent protection; a userspace daemon (`rust-speakersafetyd`) emulates a TI Smart Amp and is what prevents physical damage. If the user is running audio without it (custom distro, unfinished port), flag this first. Never suggest "just unmute to test" without confirming the daemon is running.

2. **Never touch partition 1 or the last partition on `/dev/nvme0n1`.**
   - `nvme0n1p1` = `iBootSystemContainer` (Apple Silicon boot).
   - The last partition = `RecoveryOSContainer` (Apple Silicon recovery).
   - Erasing/reformatting either can brick the machine into DFU-only state. If partition changes are needed, keep the table sorted by disk offset (`sudo sfdisk -r /dev/nvme0n1`).

3. **macOS must stay installed and bootable.** Installer, firmware upgrades, and m1n1 stage 1 reinstall all require macOS admin credentials and recoveryOS. Do not suggest "just delete the macOS partition to reclaim space" — it breaks recovery.

4. **The native page size is 16K.** The Fedora Asahi Remix `kernel-16k` is required for full hardware support. The 4K `kernel` package exists but is unsupported. Do not suggest switching page size "to run more x86 apps" — that is what `muvm` exists to solve.

5. **Don't install PulseAudio proper** — PipeWire's `pipewire-pulse` is already active and installing real PulseAudio breaks speaker output.

## Boot chain at a glance

Every boot troubleshooting question maps to one stage in this chain:

```
Power on
  └─▶ Apple SFR (System Firmware / iBoot)        ← Apple, factory; DFU restores only this
        └─▶ m1n1 stage 1 (EFI partition)         ← placed by asahi-installer; rarely updated
              └─▶ m1n1 stage 2 (distro package)  ← updated by `sudo update-m1n1`
                    └─▶ U-Boot (distro package)
                          └─▶ GRUB
                                └─▶ Linux kernel (kernel-16k)
                                      └─▶ initramfs + userspace
```

**Diagnosis shortcut:** what the user sees tells you where it stopped. See `references/boot-and-recovery.md` for the full table. Common signals:

- Stuck at `Running proxy...` → stage 2 failed to load → revert `boot.bin.old` from the EFI partition (recoverable from macOS).
- Kernel/initramfs error in U-Boot → press Escape at U-Boot countdown to enter GRUB, pick an older kernel.
- Apple logo then blank / reboot loop → stage 1 corrupted → boot recoveryOS, re-run installer, pick "Repair" (`p`) or "Upgrade m1n1" (`m`).
- Exclamation mark or SOS LED → DFU Revive needed (from another Mac via Apple Configurator, or Fedora Linux via `idevicerestore`).

## When this skill applies

Invoke on any of these signals:

- User names: Asahi, Asahi Linux, Fedora Asahi Remix, m1n1, alx.sh, fedora-asahi-remix.org.
- Hardware context: Apple Silicon / M1 / M2 / M3 / M4 MacBook / Mac mini / Mac Studio + Linux.
- Symptoms on Apple Silicon: no WiFi, quiet/distorted speakers, notch/display issues, touchbar, battery, DFU.
- Tools: `muvm`, `FEXBash`, `FEX-Emu`, `speakersafetyd`, `tiny-dfr`, `asahi-diagnose`, `update-m1n1`, `asahi-bless`, `widevine-installer`.
- Upstream work: kernel `drm/asahi`, `drivers/soc/apple/`, Apple SoC device trees, Mesa `asahi`/`honeykrisp` drivers, `m1n1` hypervisor mode for reverse-engineering.

Do **not** invoke for Asahi Guix packaging — that's the `asahi-guix` skill.

## Routing map — where to dig deeper

SKILL.md gives you the essentials. For anything more than a surface answer, load the relevant reference:

| Topic | Reference file |
|---|---|
| Won't boot, can't recover, DFU, m1n1 repair, installer flags | `references/boot-and-recovery.md` |
| Speakers, microphone, Touch Bar, firmware, hardware support matrix | `references/hardware-audio.md` |
| x86/x86-64 apps, Steam, Proton, muvm + FEX, why 4K VM | `references/x86-emulation.md` |
| GPU driver, Mesa, Vulkan, OpenGL, gaming perf, DisplayPort | `references/gpu-stack.md` |
| Contributing upstream: m1n1, kernel, Mesa, Rust userspace | `references/upstream-dev.md` |
| Canonical docs URLs, repo inventory, progress reports, community | `references/resources.md` |

## Commands worth knowing by heart

```sh
# Install (from macOS)
curl https://fedora-asahi-remix.org/install | sh        # official Fedora branding
curl https://alx.sh | sh                                # upstream Asahi branding / more options
curl https://fedora-asahi-remix.org/builds | sh         # beta / nightly builds

# Inside Fedora Asahi Remix
sudo update-m1n1                 # update stage 2 bootloader after kernel upgrades
sudo asahi-diagnose              # collect logs for bug reports (always ask for this output)
sudo fwupdmgr update             # apply firmware updates
sudo widevine-installer          # enable DRM (Netflix, Spotify — unofficial)
grubby --args=appledrm.show_notch=1 --update-kernel=ALL  # extend display above notch

# x86 emulation (muvm + FEX)
sudo dnf install fex-emu         # minimal stack
sudo dnf install steam           # full Steam wrapper (pulls everything)
muvm -- /absolute/path/to/binary # run x86 binary (absolute path required)
muvm -- FEXBash /path/to/launcher.sh
muvm -ti -- free                 # inspect VM memory from host
```

## The guiding principles to apply when helping

1. **Ground answers in the boot chain.** If the user describes a boot failure, identify the stage before prescribing a fix. Random commands without stage identification often make things worse (e.g., reinstalling stage 1 when stage 2 is the problem).

2. **Prefer supported paths.** The Asahi Installer is the only supported install path (Calamares-based installer was abandoned). `muvm` is the only supported x86 path (old "install FEX directly" tutorials are obsolete as of Fedora 42). If the user is following an older guide, say so.

3. **Check the Feature Support matrix before claiming hardware works.** The matrix at https://asahilinux.org/docs/platform/feature-support/overview/ is the source of truth; linearized feature claims in random forum posts age fast. When in doubt, cite the matrix URL rather than a stale "I think it works now."

4. **For upstream work, read the latest progress report first.** https://asahilinux.org/blog/ — the most recent Progress Report (e.g., "Linux 6.19") is where marcan/Alyssa/Asahi Lina announce what landed, what's broken, and what's merged upstream. A skill answer that contradicts the latest report is almost certainly wrong.

5. **Collecting diagnostics beats guessing.** For any non-trivial bug, ask the user to run `sudo asahi-diagnose` (part of `asahi-scripts`) and share the output. It gathers kernel version, m1n1 version, DTS, firmware state, dmesg — the things needed to give an accurate answer.

6. **When the fix is "run the installer again," say so plainly.** Many recovery paths route through `alx.sh` with option `p` (repair) or `m` (update m1n1). This is the blessed recovery and usually safer than hand-editing the EFI partition.

## Facts that frequently go stale — verify before repeating

The project moves fast. These items are current as of Apr 2026 but worth re-checking against https://asahilinux.org/blog/ before giving advice:

- **GPU driver (`drm/asahi`) is Rust and in the process of being upstreamed.** As of kernel 6.19, most Apple Silicon support is mainline; the Asahi fork primarily carries WIP (M3 GPU, DisplayPort alt-mode on the `fairydust` branch).
- **Mesa `asahi` (GL) and `honeykrisp` (Vulkan) are upstream** in mainline Mesa. There is no `AsahiLinux/mesa` repo — old guides pointing there are obsolete. Fedora still maintains a downstream `mesa` for Remix packaging but the drivers themselves come from upstream.
- **M3 basic support landed in 6.19; M3 GPU and M4 are WIP.** Check the latest progress report for the current state.
- **x86 emulation stack (FEX + muvm + binfmt-dispatcher) was integrated into Fedora Linux proper in Fedora 42.** Older "Fedora Asahi Remix-specific" phrasing is out of date.

Everything in `references/` may also have moved — if a user reports "this doesn't match the docs," trust the docs and update the skill.
