(define-module (r0man guix home packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages aspell)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages dns)
  #:use-module (gnu packages docker)
  #:use-module (gnu packages elixir)
  #:use-module (gnu packages file)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages geo)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gimp)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gnuzilla)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages inkscape)
  #:use-module (gnu packages java)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages libreoffice)
  #:use-module (gnu packages librewolf)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages man)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages node)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pv)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages search)
  #:use-module (gnu packages security-token)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages tree-sitter)
  #:use-module (gnu packages uml)
  #:use-module (gnu packages video)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages)
  #:use-module (guix utils)
  #:use-module (r0man guix packages container)
  #:export (packages))

(define packages-aarch64
  (list))

(define packages-x86-64
  (list gimp
        pandoc
        python-yubikey-manager))

(define packages-common
  (list
   `(,isc-bind "utils")
   autoconf
   automake
   ;; awscli
   bash
   c2ffi
   clang
   cmake
   coreutils
   docker-compose
   file
   flatpak
   font-dejavu
   font-fira-code
   font-gnu-freefont
   font-google-roboto
   font-hack
   font-inconsolata
   font-montserrat
   font-terminus
   fontconfig
   gcc-toolchain
   glibc-locales
   gnu-make
   gnupg
   gnutls
   graphviz
   help2man
   htop
   imagemagick
   inkscape
   ispell
   isync
   libatasmart
   libffi
   libgcrypt
   librdkafka
   libreoffice
   librewolf
   libvterm
   mu
   node-lts
   nss-certs
   mplayer
   password-store
   pgcli
   pkg-config
   plocate
   po4a
   postgis
   postgresql
   pv
   python-virtualenv
   python-yubikey-manager
   recoll
   redshift
   ripgrep
   rlwrap
   rofi
   rxvt-unicode
   sed
   sendgmail
   shared-mime-info
   sqitch
   sqlite
   strace
   texi2html
   texinfo
   tree-sitter
   tree-sitter-bash
   tree-sitter-cli
   tree-sitter-clojure
   tree-sitter-dockerfile
   tree-sitter-html
   tree-sitter-json
   tree-sitter-markdown
   tree-sitter-markdown-gfm
   tree-sitter-org
   tree-sitter-scheme
   unzip
   util-linux
   wireless-tools
   xdg-desktop-portal
   zip))

(define packages
  (cond ((target-aarch64?)
         (append packages-common packages-aarch64))
        ((target-x86-64?)
         (append packages-common packages-x86-64))
        (else packages-common)))
