# Hardware support, audio, and peripherals

This reference covers per-subsystem hardware support on Apple Silicon Linux, and how to diagnose/fix the common symptoms users report.

## Hardware support matrix — where to check

The authoritative, continuously-updated matrix lives at:

**https://asahilinux.org/docs/platform/feature-support/overview/**

This is the source of truth for whether a given feature works on M1/M1 Pro/M1 Max/M1 Ultra/M2/M2 Pro/M2 Max/M2 Ultra/M3/M4. Before telling a user "X works" or "X doesn't work," cite this page. Claims in forum threads older than ~6 months are frequently wrong in either direction.

As of Apr 2026: M1 and M2 families are first-class; M3 has basic support (kernel 6.19+); M3 GPU and M4 are actively in development. Check the latest **Progress Report** at https://asahilinux.org/blog/ for the status of anything that isn't green on the matrix.

## Audio — the most safety-critical subsystem

### Why it's different on Apple Silicon

Apple Silicon Macs have no hardware overcurrent protection for speakers. The DSP and safety logic that lives in macOS's audio stack (Apple's "smart amp" firmware) is not ported to Linux — it has to be reimplemented in userspace. Without it, a loud signal can physically damage the drivers.

### The Fedora Asahi Remix audio stack

Packages that make up "audio that sounds good and doesn't kill speakers":

| Package | Role | Upstream repo |
|---|---|---|
| `rust-speakersafetyd` | Speaker protection daemon (V/I monitoring, throttling) | AsahiLinux/speakersafetyd |
| `asahi-audio` | Per-machine DSP configs (PipeWire/LSP plugin chains) | AsahiLinux/asahi-audio |
| `rust-bankstown-lv2` | Psychoacoustic bass enhancement plugin | |
| `rust-triforce-lv2` | Microphone processing plugin | |
| `alsa-ucm-asahi` (= `alsa-ucm-conf-asahi`) | ALSA UCM overlay for Apple-specific routing | AsahiLinux/alsa-ucm-conf-asahi |
| `rust-alsa` | Rust ALSA bindings used by the above | |

All of these are installed by the `asahi-platform-metapackage`. On a stock Fedora Asahi Remix install they're present by default — the user should not need to install anything.

### Common audio symptoms and fixes

**"Speakers sound quiet/muffled/distorted" or "bass is weak":**
1. Confirm `speakersafetyd` is running: `systemctl status speakersafetyd`. If not, do **not** "fix" by disabling the service — get it running.
2. Confirm `asahi-audio` is installed: `rpm -q asahi-audio`. It provides the DSP graph PipeWire uses.
3. PipeWire should be picking the right UCM profile per machine — check `wpctl status` for the expected output device name (e.g., "MacBook Pro Speakers").
4. Absolute maximum volume is intentionally capped lower than macOS — Asahi prioritises longevity. That's not a bug.

**"No audio at all":**
- Check that **PulseAudio itself is not installed**. The system uses `pipewire-pulse` for PA compatibility. If someone installed real PulseAudio from a guide, uninstall it and reinstall `pipewire-pulse`.
- `journalctl -u speakersafetyd` — the daemon exits if the hardware signature doesn't match known-good profiles.

**"Microphone doesn't work":**
- The built-in mic uses `rust-triforce-lv2`. Confirm it's installed.
- ALSA UCM routing changed between kernels — if the user upgraded the kernel without `asahi-audio`, mic routing may be stale.

**"External audio (headphones, HDMI) works but internal speakers don't":**
- Speakers specifically require `speakersafetyd`. Other outputs don't. If only speakers are broken, the daemon is the first suspect.

### Running Asahi audio on a non-Fedora distro

The user may be on Arch Linux ARM, NixOS, or a custom setup (Guix). Warn them explicitly: **built-in speakers must not be enabled without a working `speakersafetyd` + UCM + DSP chain.** On NixOS see `nixpkgs`'s `pkgs/by-name/as/asahi-audio` / `asahi-bless` / `asahi-fwextract`. On Arch, see AsahiLinux/PKGBUILDs.

## Firmware

Firmware for WiFi, Bluetooth, speakers, neural engine, and more is extracted from macOS during installation. The installer gathers it into a tarball placed on the EFI partition; initramfs loads it on boot via `dracut-asahi` (part of `asahi-scripts`).

If firmware breaks (unlikely but possible after a macOS upgrade changes versions):

```sh
# Collect fresh firmware from the macOS install
sudo asahi-fwupdate

# Which is just a wrapper around these; asahi-scripts ships both
```

Standard Linux firmware updates (e.g., for Thunderbolt) go through `fwupd`:

```sh
sudo fwupdmgr refresh
sudo fwupdmgr get-updates
sudo fwupdmgr update
```

## Display, notch, DisplayPort

- **Internal display**: works via `apple_dcp` (now `appledrm`) DRM driver. Supports ProMotion refresh rates, HDR in a limited way, and the notch cutout.
- **"Extend above notch"**: the kernel parameter changed from `apple_dcp.show_notch` to `appledrm.show_notch`. Apply with:
  ```sh
  sudo grubby --args=appledrm.show_notch=1 --update-kernel=ALL
  ```
- **External displays via USB-C / Thunderbolt**: DisplayPort alt-mode (DP-alt) is actively being worked on in the `fairydust` branch of the kernel. Check the latest progress report before telling a user it works.
- **HDMI** on Mac mini / Mac Studio: works.
- **Thunderbolt display output**: more limited; check the feature matrix.

## Touch Bar (MacBook Pro 13"/15"/16" 2016-2019 models)

On Linux the Touch Bar is just a second small display and a touch input surface. The `rust-tiny-dfr` daemon renders a "virtual function row" that mimics what a physical row of F-keys would provide. Package: `rust-tiny-dfr` (upstream AsahiLinux/tiny-dfr).

Customizing the layout: edit `/etc/tiny-dfr/config.toml` — see the upstream README for the schema.

## Touchpad

- **"Pressing keys stops the touchpad"**: this is the "Disable while typing" touchpad feature in your DE settings, not an Asahi bug. Turn it off in KDE System Settings → Input Devices → Touchpad, or GNOME Settings → Mouse & Touchpad.

## WiFi and Bluetooth

WiFi (Broadcom BCM4387 / BCM4378) and Bluetooth work on M1/M2/M3. Firmware comes from macOS at install time (see [Firmware](#firmware)).

Two experimental Rust tools sync WiFi/Bluetooth state between macOS and Linux — neither is installed by default because NVRAM writes can race:

- `rust-asahi-wifisync` — WiFi SSIDs/credentials.
- `rust-asahi-btsync` — Bluetooth pairings.

## Battery, thermal, power

The `asahi-scripts` repo provides `macsmc-battery/` for battery and thermal reporting via the Apple SMC. Standard `upower` / `tlp` (careful — tlp profiles assume x86) work on top.

## Camera

iSight is **not yet supported** (requires ISP driver work — in progress). The ISP is one of the biggest remaining upstream gaps. Status in the Feature Support matrix.

## Widevine (DRM)

For Netflix / Spotify / other Widevine-gated content:

```sh
sudo widevine-installer
```

This downloads and extracts the Widevine CDM from a ChromeOS image. Widevine is **not** officially affiliated with Asahi or Fedora — it's a third-party binary. For Netflix specifically you'll also need a user-agent switcher; see `docs-site/modules/ROOT/pages/faq.adoc#widevine`.

## Collecting diagnostics

Any hardware bug report should start with:

```sh
sudo asahi-diagnose
```

This is a 336-line script from `asahi-scripts` that gathers:
- Apple SoC model and revision
- Kernel version + boot parameters
- m1n1 stage 1 and stage 2 versions
- DTS (device tree) and firmware state
- Relevant dmesg excerpts
- PipeWire / WirePlumber output (for audio bugs)

Ask the user to paste the output (or save to a file and share). Without this info, most hardware diagnosis is guesswork.
