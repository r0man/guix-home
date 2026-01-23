# Makefile for macOS Emacs configuration setup
# Replicates the tangling that Guix Home does at build time.

EMACS ?= emacs
BREW ?= brew

EMACS_DIR := modules/r0man/guix/home/files/emacs
INIT_ORG := $(EMACS_DIR)/init.el.org
EARLY_INIT_ORG := $(EMACS_DIR)/early-init.el.org
BOOKMARKS := $(EMACS_DIR)/bookmarks

INIT_EL := $(EMACS_DIR)/init.el
EARLY_INIT_EL := $(EMACS_DIR)/early-init.el

EMACS_D := $(HOME)/.emacs.d

.PHONY: all tangle link brew setup clean

all: tangle

## tangle: Tangle .org files into .el files using emacs --batch
tangle: $(INIT_EL) $(EARLY_INIT_EL)

$(INIT_EL): $(INIT_ORG)
	$(EMACS) --batch \
	  --eval "(require 'org)" \
	  --eval "(setq org-confirm-babel-evaluate nil)" \
	  --eval "(org-babel-tangle-file \"$(CURDIR)/$(INIT_ORG)\" \"$(CURDIR)/$(INIT_EL)\")"

$(EARLY_INIT_EL): $(EARLY_INIT_ORG)
	$(EMACS) --batch \
	  --eval "(require 'org)" \
	  --eval "(setq org-confirm-babel-evaluate nil)" \
	  --eval "(org-babel-tangle-file \"$(CURDIR)/$(EARLY_INIT_ORG)\" \"$(CURDIR)/$(EARLY_INIT_EL)\")"

## link: Symlink tangled files + bookmarks into ~/.emacs.d/
link: tangle
	mkdir -p $(EMACS_D)
	ln -sf $(CURDIR)/$(INIT_EL) $(EMACS_D)/init.el
	ln -sf $(CURDIR)/$(EARLY_INIT_EL) $(EMACS_D)/early-init.el
	ln -sf $(CURDIR)/$(BOOKMARKS) $(EMACS_D)/bookmarks

## brew: Install emacs-plus via Homebrew
brew:
	$(BREW) tap d12frosted/emacs-plus
	$(BREW) install emacs-plus --with-native-comp

## setup: Full setup -- brew install, tangle, and link
setup: brew tangle link
	@echo "Setup complete. Emacs config linked to $(EMACS_D)"
	@echo "Start Emacs and packages will auto-install from MELPA."

## clean: Remove tangled .el files
clean:
	rm -f $(INIT_EL) $(EARLY_INIT_EL)
