(define-module (r0man guix home packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages aspell)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages containers)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages dns)
  #:use-module (gnu packages docker)
  #:use-module (gnu packages file)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages geo)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gimp)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages inkscape)
  #:use-module (gnu packages libreoffice)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages man)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages node)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pv)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages search)
  #:use-module (gnu packages security-token)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages tor)
  #:use-module (gnu packages tree-sitter)
  #:use-module (gnu packages video)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages)
  #:use-module (guix utils)
  #:use-module (r0man guix packages container)
  #:use-module (r0man guix packages golang-apps)
  #:use-module (r0man guix packages task-management)
  #:export (packages-aarch64
            packages-base
            packages-desktop
            packages-x86-64))

(define packages-aarch64
  (list))

(define packages-x86-64
  (list flameshot
        gimp
        libreoffice
        nerd-dictation/xdotool
        pandoc
        python-yubikey-manager))

(define packages-base
  (list `(,isc-bind "utils")
        autoconf
        automake
        beads
        cmake
        coreutils
        ffmpeg
        file
        forgejo-cli
        gcc-toolchain
        glibc-locales
        `(,glib "bin")
        gnu-make
        gnupg
        gnutls
        graphviz
        help2man
        htop
        imagemagick
        ispell
        isync
        jq
        libatasmart
        libvterm
        mumi
        ncurses
        node-lts
        nss-certs
        openssl
        password-store
        pgcli
        pkg-config
        plocate
        podman
        podman-compose
        postgresql
        pv
        python-virtualenv
        ripgrep
        rlwrap
        sed
        sendgmail
        shared-mime-info
        sqitch
        sqlite
        strace
        texi2html
        texinfo
        tor
        tree
        tree-sitter
        tree-sitter-bash
        tree-sitter-cli
        tree-sitter-clojure
        tree-sitter-dockerfile
        tree-sitter-go
        tree-sitter-html
        tree-sitter-json
        tree-sitter-markdown
        tree-sitter-org
        tree-sitter-scheme
        unzip
        util-linux
        vivid
        vibecoder
        whisper-cpp
        wireless-tools
        zip))

(define packages-desktop
  (list flatpak
        font-adobe-source-code-pro
        font-awesome
        font-dejavu
        font-fira-code
        font-ghostscript
        font-gnu-freefont
        font-google-noto
        font-google-noto-emoji
        font-google-roboto
        font-google-roboto-mono
        font-hack
        font-inconsolata
        font-montserrat
        font-terminus
        fontconfig
        gtk+ ;; Provide org.gtk.Settings.FileChooser
        inkscape
        mplayer
        redshift
        xdg-desktop-portal
        xdg-desktop-portal-gtk))
