# x86 / x86-64 emulation: muvm + FEX

Apple Silicon runs aarch64. To run x86 and x86-64 apps — especially Windows games under Proton — Asahi uses a **microVM** (`muvm`) around the **FEX-Emu** userspace emulator.

This reference is current as of Fedora Linux 42, which integrated the stack into Fedora proper.

## Why a VM? The page-size problem

Apple Silicon's IOMMU and GPU only handle **16 KB** pages. Linux's memory management assumes the CPU page size ≥ IOMMU page size, so Fedora Asahi Remix ships `kernel-16k` (16 KB pages).

But x86 and x86-64 binaries assume **4 KB** pages everywhere — they don't work under a 16 KB page kernel.

Rather than run the whole system at 4 KB (slower, and breaks IOMMU/GPU), Asahi boots a tiny guest kernel at 4 KB inside a microVM — `muvm`. The VM:
- Shares the host's root filesystem as-is (same `/usr`, `/home`, `/etc`, etc.).
- Passes through the GPU via native context.
- Is practically invisible to applications.

FEX-Emu inside the VM JIT-translates x86 → aarch64. It uses **TSO mode** (the same Apple Silicon CPU feature Rosetta uses) for fast + accurate memory-model emulation.

## The stack

| Component | Package | Role |
|---|---|---|
| `muvm` | `muvm` | microVM runner (on libkrun) |
| FEX-Emu | `fex-emu` | x86/x86-64 JIT emulator |
| FEX RootFS | `fex-emu-rootfs-fedora` | minimal x86 userspace (libc, common libs) |
| Mesa x86 overlay | `mesa-fex-emu-overlay-i386`, `mesa-fex-emu-overlay-x86-64` | x86 builds of Asahi GL/Vulkan for emulated apps |
| binfmt-dispatcher | `rust-binfmt-dispatcher` | picks the right interpreter via binfmt_misc |
| Steam wrapper | `steam` (Fedora Asahi package) | auto-configures Proton + muvm |

Install:

```sh
sudo dnf install fex-emu           # minimal — just run your own binaries
sudo dnf install steam             # full — Steam + Proton + DXVK + vkd3d-proton
```

**Old guides are obsolete.** Anything pre-Fedora-42 that tells the user to "install FEX directly on the host," set up 4K kernels, or hand-roll binfmt_misc is stale. Always use `muvm` as the entry point.

## Running x86 things

### Steam

```sh
sudo dnf install steam
steam                              # or launch from the DE
```

The Steam wrapper script boots muvm in the background and feeds Steam into it. Everything Just Works for Steam Play titles. Enable **Menu → Settings → Compatibility → Enable Steam Play for all other titles** and restart Steam.

### Any x86 binary

```sh
muvm -- /absolute/path/to/app      # path MUST be absolute
```

Caveats:
- `muvm` does not preserve the current working directory — always pass an absolute path.
- If the app is launched by a **shell script**, wrap it in `FEXBash`:
  ```sh
  muvm -- FEXBash /absolute/path/to/launcher.sh
  ```
  That ensures the shell itself runs in the emulated environment, so subcommands the script runs behave like x86.

### Interactive shells

```sh
muvm -- bash                       # aarch64 shell inside the 4K VM
muvm -- FEXBash                    # x86-64 shell (useful for debugging)
muvm -ti -- free                   # inspect VM memory
muvm -ti -- htop
muvm -tip 3335 -- bash             # root shell in VM (for strace, debug)
```

"Root" in the VM only has the privileges of your user account — sudo does not elevate inside the VM. Use root shells for `strace` or runtime tweaks; don't try to install system packages from inside.

## Scope — what muvm+FEX is and is not for

**Designed for:** portable x86/x86-64 apps that install to `$HOME` or `/opt` — AppImages, Steam games, productivity tools.

**Not designed for:** x86 apps that need system packages, run installers as root, or expect a self-contained rootfs. The VM shares your host's root filesystem — you can't `dnf install something-x86_64` inside and have it work.

## Filesystem semantics inside the VM

Host and guest share `/usr`, `/home`, `/etc`, `/opt`, `/var`, `/tmp`, and most other root-level dirs. Exceptions:

- `/dev`, `/sys`, `/proc` — guest-private.
- `/dev/shm` — shared (enables X11 forwarding).
- `/run` — guest-private.
- `/run/fex-emu/rootfs` — FEX's x86 RootFS, visible to emulated apps only.
- `/run/muvm-host` — the entire host filesystem view (so from guest you can `ls /run/muvm-host/run` to see host `/run`).
- `/tmp/.X11-unix` — guest-private tmpfs.

Mounts added on the host after VM start **appear lazily**: they only show up in `mount` output inside the guest after something in the mountpoint is accessed. This is intentional.

## Hardware access inside the VM

Passed through via software protocols, not hardware-level passthrough:

- **GPU** — native context passthrough; same performance as host.
- **X11 display, keyboard, mouse** — XWayland proxy.
- **Gamepads** — `hid/uinput` passthrough via hidpipe-successor integration.
- **Audio** — PulseAudio socket protocol (works because `pipewire-pulse` is on the host). Do **not** install real PulseAudio on the host.

Not passed through:
- USB devices
- Webcams (being researched — future PipeWire passthrough)
- PCIe, Thunderbolt devices

## Known quirks

### Steam "steamwebhelper crashed"

Happens on first launch because webhelper startup is slow under emulation and Steam's timeout fires. Dismiss and let it restart — it works on the second try.

### Games missing libraries

```sh
dnf download --repo=fedora --repo=updates --forcearch=x86_64 --best <package>
# then rpmdev-extract and LD_LIBRARY_PATH

# Or overlay an RPM into FEX RootFS:
rpm2archive -n mypackage.rpm
mkfs.erofs --tar=f mypackage.rpm.erofs mypackage.rpm.tar
muvm -f /usr/share/fex-emu/RootFS/default.erofs \
     -f /usr/share/fex-emu/overlays/mesa-x86_64.erofs \
     -f /usr/share/fex-emu/overlays/mesa-i386.erofs \
     -f mypackage.rpm.erofs \
     <muvm args>
```

If the missing library is common and small, submit a PR to https://pagure.io/fedora-kiwi-descriptions to have it added to the default FEX RootFS.

### Browsers inside the guest are dangerous

Guest browser instances **cannot** safely share a profile with host instances. `muvm` forces Firefox to use a dedicated profile inside the VM. If the default browser is something else, close all host browser windows before running muvm-launched apps that might open a browser, or configure separate profiles manually.

### External drives at `/run/media/...` don't work in the VM

POSIX ACL support is missing in libkrun. Workaround: mount the drive manually at e.g. `/mnt/steam` (with `sudo chown $USER: /mnt/steam`).

### Steam external library

```sh
sudo mount /dev/sdX1 /mnt/steam            # ext4 — FAT/exFAT won't work
sudo chown $USER: /mnt/steam
mkdir /mnt/steam/steamapps
# Then in Steam → Settings → Storage → Add Drive → /mnt/steam
```

Note: **don't** put the drive in `/etc/fstab` — if it's disconnected, the system won't boot.

## Performance tuning

### WineD3D vs DXVK for DX8–DX11 games

DXVK (default) converts DirectX 8–11 → Vulkan. WineD3D converts → OpenGL. The Asahi OpenGL driver has optimizations Vulkan doesn't yet have, so some DX8–DX11 games are faster on WineD3D:

```
Steam → game → Properties → Launch options:
PROTON_USE_WINED3D=1 %command%
```

DXVK tends to have better compatibility, so try DXVK first.

### 32-bit games with heavy x87 usage

Older games using 80-bit x87 floats are slow because FEX emulates them in software for accuracy (same issue Rosetta has). For games that don't need precise rounding:

```
FEX_X87REDUCEDPRECISION=1 %command%
```

### VM memory usage

By default muvm allows up to 80% of host RAM. The VM shrinks its page cache under host memory pressure. To cap:

```sh
muvm --mem=8G -- app
```

### CPU cores

By default, muvm pins vCPUs to host **performance cores** only (P-cores, not E-cores). Change with `muvm --cpu-list=CPU_LIST`.

## FAQ-level factoids users ask

- **"Is this like Rosetta?"** — Technically similar (both JIT + TSO). Difference: Rosetta sidesteps the page-size problem via XNU's mixed page support; Asahi must use a VM.
- **"Why not box64?"** — FEX prioritizes correctness; box64 prioritizes "out of the box" breadth. Both work; Asahi picked FEX. `box64` is packaged in Fedora — users can try it.
- **"Does Wayland work in the VM?"** — Not yet. XWayland is used. Most legacy x86 apps are X11 anyway.
- **"Can I run Windows apps outside Steam?"** — Not yet — non-Proton Wine needs https://github.com/FEX-Emu/FEX/pull/4225 resolved. In the meantime, add the .exe as a non-Steam game.
- **"Webcam?"** — Not yet.
