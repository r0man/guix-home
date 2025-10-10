# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working
with code in this repository.

## Overview

This repository contains GNU Guix Home configuration for managing user
environments across multiple systems. It uses Guix's declarative
configuration approach to manage packages, services, and dotfiles across
different machines (M1 MacBook and Dell Precision laptop).

## Architecture

### Module Organization

The codebase follows Guix's module structure under `modules/r0man/guix/`:

- **`home/`** - Home environment modules (services and configurations)
  - Each file exports service lists (e.g., `home-bash-services`,
    `home-emacs-services`)
  - Files include both service definitions and package lists
  - `packages.scm` defines architecture-specific packages
    (`packages-aarch64`, `packages-x86-64`, `packages-base`,
    `packages-desktop`)
  - `files/` subdirectory contains dotfiles and configuration files

- **`home/systems/`** - System-specific configurations
  - `m1.scm` - M1 MacBook configuration (aarch64)
  - `precision.scm` - Dell Precision laptop configuration (x86-64)
  - `server.scm` - Server configuration
  - Each system file imports relevant service modules and composes them

- **`system/`** - System-level configurations (not home environment)

- **`deploy/`** - Deployment configurations

### Service Pattern

Modules in `home/` follow one of two patterns:

**1. Service Type Pattern** (most modules):
```scheme
(define-module (r0man guix home <name>)
  #:use-module (gnu home services ...)
  #:use-module (guix records)
  #:export (home-<name>-configuration
            home-<name>-service-type))

(define-record-type* <home-<name>-configuration>
  home-<name>-configuration make-home-<name>-configuration
  home-<name>-configuration?
  ...)

(define home-<name>-service-type
  (service-type
   (name 'home-<name>)
   (extensions ...)
   (default-value (home-<name>-configuration))
   (description "...")))
```

System configs instantiate with: `(service home-<name>-service-type)`

**2. Wrapper Function Pattern** (for custom configurations):
```scheme
(define-module (r0man guix home <name>)
  #:use-module (gnu home services ...)
  #:use-module (guix records)
  #:export (home-<name>-custom-configuration
            make-home-<name>-services))

(define-record-type* <home-<name>-custom-configuration> ...)

(define* (make-home-<name>-services #:optional (config ...))
  "Create list of services from configuration."
  (list (service ...)))
```

System configs instantiate with: `(make-home-<name>-services)`

**3. Service List Pattern** (for multi-service modules):
```scheme
(define-module (r0man guix home <name>)
  #:export (home-<name>-services))

(define home-<name>-services
  (list (service ...) (service ...)))
```

System configs use directly: `home-<name>-services`

System configurations in `home/systems/` compose services using `append`:
- Use `(service home-<name>-service-type)` for service types
- Use `(make-home-<name>-services)` or `(make-home-<name>-services config)`
  for wrappers
- Use `home-<name>-services` directly for service lists

### Channel Dependencies

This repository depends on four Guix channels (defined in
`modules/r0man/guix/channels.scm`):
- `guix` - Official GNU Guix channel
- `nonguix` - Non-free software (e.g., proprietary firmware)
- `asahi` - Asahi Linux packages for Apple Silicon
- `r0man` - Personal channel at github.com/r0man/guix-channel

## Commands

### Building and Testing

Test configuration without applying changes:
```bash
guix home -L modules --dry-run reconfigure modules/r0man/guix/home/systems/m1.scm
guix home -L modules --dry-run reconfigure modules/r0man/guix/home/systems/precision.scm
```

Apply home environment configuration:
```bash
guix home -L modules reconfigure modules/r0man/guix/home/systems/m1.scm
guix home -L modules reconfigure modules/r0man/guix/home/systems/precision.scm
```

### Channel Management

Configure channels (one-time setup):
```bash
mkdir -p ~/.config/guix/
rm -f ~/.config/guix/channels.scm
ln -s "$(pwd)/modules/r0man/guix/channels.scm" ~/.config/guix/channels.scm
```

Update channels and packages:
```bash
guix pull
```

### Development

Search for packages:
```bash
guix search <package-name>
```

Check module imports and syntax:
```bash
guix repl
,use (r0man guix home packages)
```

## Key Implementation Details

### Architecture-Specific Packages

The `packages.scm` module uses `target-aarch64?` and `target-x86-64?`
predicates to conditionally include packages:
- aarch64 (M1): Uses `emacs-pgtk` (pure GTK Emacs)
- x86-64 (Precision): Includes `gimp`, `pandoc`, `python-yubikey-manager`

### Service Composition

System configurations use `append` to compose service lists from multiple
modules. The pattern allows modular service definitions while keeping
system-specific configurations simple.

### File Management

Configuration files are managed via `local-file` in service definitions,
pointing to `files/` subdirectories. Files are symlinked into `$HOME`
during `guix home reconfigure`.

### Emacs Configuration

Emacs is managed through Guix packages (not package.el). The
configuration includes:
- ~200+ Emacs packages declared in `home/emacs.scm`
- Emacs Lisp early init file `files/emacs/early-init.el`
- Literate config in `files/emacs/README.org` which becomes `init.el` when untangled

## Workflow Notes

When adding new services:
1. Create module in `modules/r0man/guix/home/<name>.scm`
2. Export `home-<name>-services` list
3. Import and append to system configuration in `home/systems/*.scm`
4. Run dry-run to validate
5. Apply with `guix home reconfigure`

When adding packages:
- Base packages (all systems): Add to `packages-base` in `packages.scm`
- Desktop packages: Add to `packages-desktop`
- Architecture-specific: Add to `packages-aarch64` or `packages-x86-64`
- Service-specific: Add within the service module's package list
