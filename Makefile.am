bin_SCRIPTS =

# Handle substitution of fully-expanded Autoconf variables.
do_subst = $(SED)					\
  -e 's,[@]GUILE[@],$(GUILE),g'				\
  -e 's,[@]guilemoduledir[@],$(guilemoduledir),g'	\
  -e 's,[@]guileobjectdir[@],$(guileobjectdir),g'	\
  -e 's,[@]localedir[@],$(localedir),g'

nodist_noinst_SCRIPTS = \
  pre-inst-env

GOBJECTS = $(SOURCES:%.scm=%.go)

moddir=$(prefix)/share/guile/site/$(GUILE_EFFECTIVE_VERSION)
godir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache
ccachedir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache

nobase_mod_DATA = $(SOURCES) $(NOCOMP_SOURCES)
nobase_go_DATA = $(GOBJECTS)

# Make sure source files are installed first, so that the mtime of
# installed compiled files is greater than that of installed source
# files.  See
# <http://lists.gnu.org/archive/html/guile-devel/2010-07/msg00125.html>
# for details.
guile_install_go_files = install-nobase_goDATA
$(guile_install_go_files): install-nobase_modDATA

GUILE_WARNINGS = -Wunbound-variable -Warity-mismatch -Wformat
SUFFIXES = .scm .go
# -L ordering: $(top_builddir)/modules first so already-compiled .go
# files are found, then $(top_srcdir)/modules so VPATH (out-of-tree)
# builds resolve sources.  R0MAN_EXTRA_LOAD_PATH is honoured by
# pre-inst-env so cross-channel deps (asahi-guix) resolve.
.scm.go:
	$(AM_V_GEN)$(top_builddir)/pre-inst-env $(GUILE_TOOLS) compile $(GUILE_WARNINGS) -L "$(top_builddir)/modules" -L "$(top_srcdir)/modules" -o "$@" "$<"

# Non-Scheme files shipped alongside the modules: home-environment
# templates, wallpapers, dotfiles, scripts, signing keys.  Listed
# explicitly (no wildcards) so `make dist` and `make install` are
# deterministic.
NOCOMP_SOURCES = \
  modules/r0man/guix/home/files/Xresources \
  modules/r0man/guix/home/files/Xresources.m1 \
  modules/r0man/guix/home/files/Xresources.precision \
  modules/r0man/guix/home/files/bash_logout \
  modules/r0man/guix/home/files/bash_profile \
  modules/r0man/guix/home/files/bashrc \
  modules/r0man/guix/home/files/bin/container-claude \
  modules/r0man/guix/home/files/bin/container-emacs \
  modules/r0man/guix/home/files/bin/container-gastown \
  modules/r0man/guix/home/files/bin/container-run \
  modules/r0man/guix/home/files/bin/nerd-dictation-toggle \
  modules/r0man/guix/home/files/claude-code/agents/.gitkeep \
  modules/r0man/guix/home/files/claude-code/agents/code-reviewer.md \
  modules/r0man/guix/home/files/claude-code/agents/documentation-writer.md \
  modules/r0man/guix/home/files/claude-code/agents/security-audit.md \
  modules/r0man/guix/home/files/claude-code/commands/create-guix-package.md \
  modules/r0man/guix/home/files/claude-code/commands/plan-to-beads.md \
  modules/r0man/guix/home/files/claude-code/commands/update-guix-package.md \
  modules/r0man/guix/home/files/claude-code/settings.json \
  modules/r0man/guix/home/files/claude-code/skills/asahi-linux/SKILL.md \
  modules/r0man/guix/home/files/claude-code/skills/asahi-linux/evals/evals.json \
  modules/r0man/guix/home/files/claude-code/skills/asahi-linux/references/boot-and-recovery.md \
  modules/r0man/guix/home/files/claude-code/skills/asahi-linux/references/gpu-stack.md \
  modules/r0man/guix/home/files/claude-code/skills/asahi-linux/references/hardware-audio.md \
  modules/r0man/guix/home/files/claude-code/skills/asahi-linux/references/resources.md \
  modules/r0man/guix/home/files/claude-code/skills/asahi-linux/references/upstream-dev.md \
  modules/r0man/guix/home/files/claude-code/skills/asahi-linux/references/x86-emulation.md \
  modules/r0man/guix/home/files/claude-code/skills/autoresearch/SKILL.md \
  modules/r0man/guix/home/files/claude-code/skills/autoresearch/evals/evals.json \
  modules/r0man/guix/home/files/claude-code/skills/guix/SKILL.md \
  modules/r0man/guix/home/files/claude-code/skills/haunt/SKILL.md \
  modules/r0man/guix/home/files/claude-code/skills/haunt/references/api.md \
  modules/r0man/guix/home/files/claude-code/skills/haunt/references/examples.md \
  modules/r0man/guix/home/files/claude-code/skills/haunt/references/themes.md \
  modules/r0man/guix/home/files/claude-code/skills/tmux/SKILL.md \
  modules/r0man/guix/home/files/claude-code/skills/tmux/references/direct-socket-control.md \
  modules/r0man/guix/home/files/claude-code/skills/tmux/references/session-lifecycle.md \
  modules/r0man/guix/home/files/claude-code/skills/tmux/references/session-registry.md \
  modules/r0man/guix/home/files/claude-code/skills/tmux/tools/cleanup-sessions.sh \
  modules/r0man/guix/home/files/claude-code/skills/tmux/tools/create-session.sh \
  modules/r0man/guix/home/files/claude-code/skills/tmux/tools/find-sessions.sh \
  modules/r0man/guix/home/files/claude-code/skills/tmux/tools/kill-session.sh \
  modules/r0man/guix/home/files/claude-code/skills/tmux/tools/lib/registry.sh \
  modules/r0man/guix/home/files/claude-code/skills/tmux/tools/lib/time_utils.sh \
  modules/r0man/guix/home/files/claude-code/skills/tmux/tools/list-sessions.sh \
  modules/r0man/guix/home/files/claude-code/skills/tmux/tools/pane-health.sh \
  modules/r0man/guix/home/files/claude-code/skills/tmux/tools/safe-send.sh \
  modules/r0man/guix/home/files/claude-code/skills/tmux/tools/wait-for-text.sh \
  modules/r0man/guix/home/files/clojure-lsp.edn \
  modules/r0man/guix/home/files/dunstrc \
  modules/r0man/guix/home/files/eca/AGENTS.md \
  modules/r0man/guix/home/files/eca/commands/create-guix-package.md \
  modules/r0man/guix/home/files/eca/commands/update-guix-package.md \
  modules/r0man/guix/home/files/eca/config.json \
  modules/r0man/guix/home/files/emacs/README.md \
  modules/r0man/guix/home/files/emacs/bookmarks \
  modules/r0man/guix/home/files/emacs/early-init.el.org \
  modules/r0man/guix/home/files/emacs/init.el.org \
  modules/r0man/guix/home/files/fzf/shell/key-bindings.bash \
  modules/r0man/guix/home/files/guile \
  modules/r0man/guix/home/files/librewolf/librewolf.overrides.cfg \
  modules/r0man/guix/home/files/niri/config-common.kdl \
  modules/r0man/guix/home/files/niri/config-m1.kdl \
  modules/r0man/guix/home/files/niri/config-precision.kdl \
  modules/r0man/guix/home/files/rofi/config.rasi \
  modules/r0man/guix/home/files/rofi/nord.rasi \
  modules/r0man/guix/home/files/sbclrc \
  modules/r0man/guix/home/files/ssh/roman@burningswell.com.pub \
  modules/r0man/guix/home/files/ssh/roman@precision.pub \
  modules/r0man/guix/home/files/stumpwm/commands.lisp \
  modules/r0man/guix/home/files/stumpwm/config \
  modules/r0man/guix/home/files/stumpwm/polybar.lisp \
  modules/r0man/guix/home/files/stumpwm/start \
  modules/r0man/guix/home/files/stumpwm/time.lisp \
  modules/r0man/guix/home/files/tmux.conf \
  modules/r0man/guix/home/files/wallpapers/guix-test-pilot.svg \
  modules/r0man/guix/home/files/wallpapers/stop-customizing-start-working.jpg \
  modules/r0man/guix/home/files/waybar/config.jsonc \
  modules/r0man/guix/home/files/waybar/style.css \
  modules/r0man/guix/home/files/xbindkeysrc \
  modules/r0man/guix/home/files/xinitrc \
  modules/r0man/guix/home/files/xprofile \
  modules/r0man/guix/home/files/xsession \
  modules/r0man/guix/system/keys/asahi-guix.pub \
  modules/r0man/guix/system/keys/nonguix.pub \
  modules/r0man/guix/system/keys/precision.pub

# All Guile modules in the channel.  Listed explicitly (no wildcards)
# so `make dist` and `make install` are deterministic and adding a new
# module forces a Makefile.am edit (which is the desired
# code-review signal).
SOURCES = \
  modules/r0man/guix/build/marionette.scm \
  modules/r0man/guix/channels.scm \
  modules/r0man/guix/deploy/m1.scm \
  modules/r0man/guix/home/bash.scm \
  modules/r0man/guix/home/channels.scm \
  modules/r0man/guix/home/desktop.scm \
  modules/r0man/guix/home/environments/container.scm \
  modules/r0man/guix/home/environments/gastown.scm \
  modules/r0man/guix/home/environments/m1.scm \
  modules/r0man/guix/home/environments/precision.scm \
  modules/r0man/guix/home/environments/server.scm \
  modules/r0man/guix/home/gpg.scm \
  modules/r0man/guix/home/msmtp.scm \
  modules/r0man/guix/home/packages.scm \
  modules/r0man/guix/home/pm.scm \
  modules/r0man/guix/home/services/btop.scm \
  modules/r0man/guix/home/services/claude-code.scm \
  modules/r0man/guix/home/services/clojure.scm \
  modules/r0man/guix/home/services/common-lisp.scm \
  modules/r0man/guix/home/services/container-home.scm \
  modules/r0man/guix/home/services/eca.scm \
  modules/r0man/guix/home/services/emacs.scm \
  modules/r0man/guix/home/services/environment.scm \
  modules/r0man/guix/home/services/fzf.scm \
  modules/r0man/guix/home/services/gastown.scm \
  modules/r0man/guix/home/services/ghostty.scm \
  modules/r0man/guix/home/services/git.scm \
  modules/r0man/guix/home/services/guile.scm \
  modules/r0man/guix/home/services/hyprland.scm \
  modules/r0man/guix/home/services/i3status.scm \
  modules/r0man/guix/home/services/kitty.scm \
  modules/r0man/guix/home/services/librewolf.scm \
  modules/r0man/guix/home/services/mbsync.scm \
  modules/r0man/guix/home/services/niri.scm \
  modules/r0man/guix/home/services/nix.scm \
  modules/r0man/guix/home/services/rofi.scm \
  modules/r0man/guix/home/services/stumpwm.scm \
  modules/r0man/guix/home/services/sway.scm \
  modules/r0man/guix/home/services/waybar.scm \
  modules/r0man/guix/home/services/wofi.scm \
  modules/r0man/guix/home/services/x11.scm \
  modules/r0man/guix/home/sound.scm \
  modules/r0man/guix/home/ssh.scm \
  modules/r0man/guix/home/tmux.scm \
  modules/r0man/guix/home/xdg.scm \
  modules/r0man/guix/services/gastown.scm \
  modules/r0man/guix/system/base.scm \
  modules/r0man/guix/system/desktop.scm \
  modules/r0man/guix/system/m1-test.scm \
  modules/r0man/guix/system/m1.scm \
  modules/r0man/guix/system/services/auditd.scm \
  modules/r0man/guix/system/services/avahi.scm \
  modules/r0man/guix/system/services/bluetooth.scm \
  modules/r0man/guix/system/services/certbot.scm \
  modules/r0man/guix/system/services/console-font.scm \
  modules/r0man/guix/system/services/containerd.scm \
  modules/r0man/guix/system/services/cups.scm \
  modules/r0man/guix/system/services/docker.scm \
  modules/r0man/guix/system/services/elogind.scm \
  modules/r0man/guix/system/services/guix-publish.scm \
  modules/r0man/guix/system/services/keyboard.scm \
  modules/r0man/guix/system/services/libvirt.scm \
  modules/r0man/guix/system/services/nix.scm \
  modules/r0man/guix/system/services/openssh.scm \
  modules/r0man/guix/system/services/pcscd.scm \
  modules/r0man/guix/system/services/postgresql.scm \
  modules/r0man/guix/system/services/qemu.scm \
  modules/r0man/guix/system/services/screen-locker.scm \
  modules/r0man/guix/system/services/slim.scm \
  modules/r0man/guix/system/services/substitutes.scm \
  modules/r0man/guix/system/services/udev-fido2.scm \
  modules/r0man/guix/system/services/xorg.scm \
  modules/r0man/guix/tests.scm \
  modules/r0man/guix/tests/home/bash.scm \
  modules/r0man/guix/tests/runner.scm \
  modules/r0man/guix/tests/systems/base.scm

# Regular, fast SRFI-64 tests — run by `make check`.
UNIT_TESTS = \
  tests/r0man/guix/smoke.scm \
  tests/r0man/guix/home/environments.scm \
  tests/r0man/guix/home/ghostty.scm

# VM-based tests — run by `make check-system` with
# R0MAN_RUN_SYSTEM_TESTS=1.  Each boots a QEMU VM under marionette,
# which is slow and requires a working Guix daemon (and KVM for
# acceptable speed).
SYSTEM_TEST_FILES = \
  tests/r0man/guix/home/bash.scm \
  tests/r0man/guix/systems/base.scm

TESTS = $(UNIT_TESTS)

TEST_EXTENSIONS = .scm
SCM_LOG_DRIVER =                                                \
  $(top_builddir)/pre-inst-env                                  \
  $(GUILE) --no-auto-compile -e main                            \
      $(top_srcdir)/build-aux/test-driver.scm

# Tell 'build-aux/test-driver.scm' to display only source file names,
# not individual test names.
AM_SCM_LOG_DRIVER_FLAGS = --brief=yes

AM_SCM_LOG_FLAGS = --no-auto-compile -L "$(top_srcdir)"

AM_TESTS_ENVIRONMENT = abs_top_srcdir="$(abs_top_srcdir)"

ACLOCAL_AMFLAGS = -I m4

EXTRA_DIST =					\
  bootstrap					\
  pre-inst-env.in				\
  build-aux/regen-ghostty-golden.scm		\
  build-aux/test-driver.scm			\
  m4/guile.m4					\
  .guix-channel					\
  .guix-authorizations				\
  .gitignore					\
  README.org					\
  tests/data/ghostty/golden-minimal.config	\
  tests/data/ghostty/golden-heavy.config	\
  $(SOURCES)					\
  $(NOCOMP_SOURCES)				\
  $(UNIT_TESTS)					\
  $(SYSTEM_TEST_FILES)

# Regenerate ghostty golden fixtures from the schema.  Re-run after
# changing the seed configurations in tests/r0man/guix/home/ghostty.scm
# or build-aux/regen-ghostty-golden.scm.
ghostty-golden:
	$(top_builddir)/pre-inst-env $(GUILE) --no-auto-compile	\
	    -L "$(top_builddir)/modules" -L "$(top_srcdir)/modules"	\
	    -s "$(top_srcdir)/build-aux/regen-ghostty-golden.scm"
.PHONY: ghostty-golden

# Run VM-based system tests (requires QEMU and a Guix daemon).
# Sets R0MAN_RUN_SYSTEM_TESTS=1 so the gated SRFI-64 tests under
# tests/r0man/guix/{home,systems}/ actually execute, and overrides
# TESTS so only those files run.
# Usage: make check-system
#        SYSTEM_TESTS_FILTER=tests/r0man/guix/systems/base.scm make check-system
check-system:
	@if test -z "$(SYSTEM_TEST_FILES)"; then				\
	  echo "make check-system: no system tests defined; skipping.";		\
	else									\
	  R0MAN_RUN_SYSTEM_TESTS=1 $(MAKE) check					\
	    TESTS="$(if $(SYSTEM_TESTS_FILTER),$(SYSTEM_TESTS_FILTER),$(SYSTEM_TEST_FILES))"; \
	fi
.PHONY: check-system

clean-go:
	-$(RM) $(GOBJECTS)
.PHONY: clean-go

CLEANFILES =					\
  $(GOBJECTS)					\
  $(UNIT_TESTS:tests/%.scm=%.log)		\
  $(UNIT_TESTS:tests/%.scm=%.trs)		\
  $(SYSTEM_TEST_FILES:tests/%.scm=%.log)	\
  $(SYSTEM_TEST_FILES:tests/%.scm=%.trs)
