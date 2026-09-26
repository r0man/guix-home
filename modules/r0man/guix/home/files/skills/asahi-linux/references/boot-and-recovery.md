# Boot troubleshooting and recovery

Every Asahi boot problem maps to one stage of the boot chain. Identify the stage from what the user observes, then apply the right fix.

## Diagnostic table

| What the user sees | Stage that failed | Go to |
|---|---|---|
| Mac boots into macOS instead of Linux | Boot Picker (user action) | [Changing the startup OS](#boot-picker) |
| `Running proxy...` then hang (black screen, m1n1 logo at top) | m1n1 stage 2 | [Reverting stage 2](#stage-2) |
| U-Boot countdown, then kernel/initramfs error | Kernel / GRUB | [GRUB fallback](#grub) |
| Stuck in U-Boot console | U-Boot (usually USB issue) | [U-Boot](#uboot) |
| Apple logo then blank screen, reboot loop | m1n1 stage 1 | [Stage 1 repair](#stage-1) |
| Exclamation mark icon / SOS LED blink | Apple SFR | [DFU Revive/Restore](#dfu) |

## Changing the startup OS {#boot-picker}

Full power-off, then hold the power button until "Entering startup options" (laptops/iMacs) or the power LED dims (desktops). The Boot Picker appears. Clicking a boot option is temporary; hold Option while clicking to make it the default.

The rightmost "Options" icon enters **recoveryOS** — you'll be asked for macOS credentials. Once in recoveryOS, Shift-Cmd-T opens a Terminal.

## Entering GRUB {#grub}

Server and Minimal editions show GRUB by default. Desktop (KDE, GNOME) editions boot through silently. To interrupt:

1. Watch the boot: m1n1 logo → U-Boot text screen (brief countdown).
2. Press Escape **immediately after the U-Boot countdown hits 0**.

Use GRUB to pick an older kernel (common after a broken kernel upgrade) or edit kernel args for single-user recovery.

## m1n1 stage 2 fails: `Running proxy...` hang {#stage-2}

Typical cause: stage 2 binary was corrupted during an update, or a new stage 2 shipped with a DT change incompatible with the running kernel. **Recoverable from macOS or recoveryOS** — no DFU needed.

From macOS:

```sh
# List EFI partitions
diskutil list | grep EFI

# Mount (replace disk0sX with the FEDOR one)
sudo diskutil mount disk0sX

# Swap the files
cd "/Volumes/EFI - FEDOR/m1n1"
ls                                  # expect boot.bin and boot.bin.old
mv boot.bin boot.bin.new
mv boot.bin.old boot.bin
```

Reboot into Linux. After a successful boot, run `sudo update-m1n1` to upgrade again. If reverting doesn't work, a DT mismatch is likely — boot GRUB and pick an older kernel that matches the older stage 2.

## U-Boot hangs or won't accept keyboard {#uboot}

U-Boot only supports **one** keyboard and struggles with certain USB devices.

- Disconnect all USB devices except a single keyboard (desktops).
- Some USB mice, barcode readers, and security tokens present as keyboards and confuse U-Boot. YubiKeys are already allowlisted.
- Try a different port or adapter (C-to-A adapters often work better than native A ports in U-Boot).

## Repairing m1n1 stage 1 {#stage-1}

Stage 1 lives in the **Boot Policy**, not in an ordinary partition. Reinstalling requires the installer again. Common triggers:

- macOS upgrade corrupted the Boot Policy (especially older macOS bugs).
- macOS Sonoma 14.5+ with m1n1 stage 1 older than **1.4.13** (May 2024) — forces an update.
- Machine returned from Apple service.

Steps:

1. Boot recoveryOS (preferred) or macOS (see [Boot Picker](#boot-picker)).
2. Ensure internet.
3. Run the installer again:
   ```sh
   curl https://fedora-asahi-remix.org/install | sh
   ```
4. When the menu appears, choose **`p` — Repair an incomplete installation** or **`m` — Upgrade m1n1 on an existing OS**.

If neither option appears (Boot Policy fully wiped), run `bputil -f`, pick the UUID matching the Linux install, and follow prompts to reset Boot Policy. This puts the install into "incomplete" state; re-run the installer and the `p` option will now appear.

## Unbootable machine: DFU Revive or Restore {#dfu}

An exclamation-mark icon (laptops/iMacs) or SOS LED blink (desktops) means the Apple System Firmware itself is broken. Recovery requires another machine.

**DFU Revive** — reinstalls SFR + recoveryOS, **preserves data**.
**DFU Restore** — wipes the entire NVMe, returns to factory state.

### From another Mac

Use Apple's own docs: https://support.apple.com/en-us/108900 — Finder or Apple Configurator 2 drive the process.

### From a Fedora Linux machine (x86 or Apple Silicon)

`idevicerestore` is packaged in Fedora and can DFU an Apple Silicon Mac from any Fedora machine.

```sh
sudo dnf install -y idevicerestore usbmuxd
sudo udevadm control --reload   # if usbmuxd was newly installed
sudo dmesg -w                   # watch in another terminal while triggering DFU
```

**Cable matters:** USB 2.0 or 3.0 data cable, Type A↔C or C↔C. **Thunderbolt 3 cables will not work.** Charge-only cables will not work.

**Target-machine port matters** (always a specific Type C):
- Laptop: leftmost (rearmost) Type C on the **left** side.
- iMac (rear view): rightmost Type C (nearest power).
- Mac mini (rear view): leftmost Type C.
- Mac Studio (rear view): rightmost Type C.
- Mac Pro tower (top view): Type C farthest from power button.
- Mac Pro rack (front view): Type C closest to power button.

**Enter DFU:**

*Desktop*: unplug mains 30 s, hold power button, plug in mains while still holding, keep holding and watch `dmesg` for `Product: Apple Mobile Device (DFU Mode)`, release when seen.

*Laptop method 1 (from off)*: tap power briefly, then immediately hold **left Ctrl + left Option + right Shift + power** for 10 s, release all but power, keep holding power and watch `dmesg`.

*Laptop method 2 (from on)*: hold **left Ctrl + left Option + right Shift + power**, wait until screen turns off, keep holding 5 more seconds, release all but power, watch `dmesg`.

If the log says `Recovery Mode` instead of `DFU Mode`, retry.

**Run the restore:**

```sh
# DFU Revive — preserves data
systemd-inhibit sudo -s TMPDIR=$PWD idevicerestore -l

# DFU Restore — wipes everything
systemd-inhibit sudo -s TMPDIR=$PWD idevicerestore -l -e
```

Select `1` when prompted for firmware version. Needs ~40 GB free in `$PWD`.

**Expected aftermath:** DFU Revive leaves the Linux install in place but breaks its boot — after Revive, redo the [Stage 1 repair](#stage-1) steps to get Linux booting again.

### Live-boot Fedora on any x86 PC to DFU

If you don't have a Fedora install handy, a Fedora Workstation live USB (≥64 GB) works. Because the live image runs from RAM, `idevicerestore`'s temp space (~40 GB) won't fit; the docs-site `troubleshooting.adoc` page has the full sfdisk recipe to carve an ext4 partition out of the USB disk's free space and mount it at `/mnt/tmp`.

## Reinstalling from scratch

If boot recovery can't salvage the install, the Asahi Installer can reinstall without a full DFU. From recoveryOS Terminal, run the same installer URL. The menu includes **`i` — install new OS** and **`u` — update an existing OS** options.

## Disk safety recap

When a question involves partitioning:

- **Never** modify `/dev/nvme0n1p1` (iBootSystemContainer) or the last partition (RecoveryOSContainer).
- Keep the table sorted by disk offset: `sudo sfdisk -r /dev/nvme0n1`.
- Avoid shrinking/modifying the macOS APFS container — the installer and firmware upgrades need it.
- External storage: not officially supported; Apple Silicon can't boot from external drives so at minimum stage 1 must stay on internal.

## When to send the user upstream

- If the issue is a hardware regression after a kernel update → https://pagure.io/fedora-asahi/remix-bugs/issues (Fedora Asahi Remix bug tracker).
- If it's upstream kernel/GPU/firmware behavior → https://github.com/AsahiLinux/linux/issues after reproducing on a recent kernel.
- Discussion: https://discussion.fedoraproject.org/c/neighbors/asahi/92 — this is where official triage happens, replacing the old Discourse.
