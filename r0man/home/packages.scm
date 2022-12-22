(define-module (r0man home packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages elixir)
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
  #:use-module (gnu packages inkscape)
  #:use-module (gnu packages java)
  #:use-module (gnu packages libffi)
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
  #:use-module (gnu packages uml)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages video)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages)
  #:use-module (r0man packages container)
  #:export (packages))

(define packages
  (list
   ;; (list openjdk11 "jdk")
   autoconf
   automake
   awscli
   clang
   cmake
   container-structure-test
   elixir
   flatpak
   font-dejavu
   font-fira-code
   font-gnu-freefont
   font-google-roboto
   font-hack
   font-inconsolata
   font-terminus
   fontconfig
   gcc-toolchain
   gdal
   gfortran-toolchain
   gimp
   git
   git-crypt
   glibc-locales
   gnu-make
   gnupg
   gnuplot
   gnutls
   graphviz
   help2man
   htop
   inkscape
   isync
   lapack
   libatasmart
   libffi
   libgcrypt
   librdkafka
   libvterm
   mu
   node
   nss-certs
   mplayer
   openblas
   openblas-ilp64
   pandoc
   password-store
   pgcli
   pkg-config
   plantuml
   plocate
   po4a
   postgis
   postgresql
   python-virtualenv
   python-yubikey-manager
   qgis
   redshift
   ripgrep
   rlwrap
   rofi
   rxvt-unicode
   sed
   shared-mime-info
   sqitch
   sqlite
   strace
   texi2html
   texinfo
   unzip
   wireless-tools
   zip))
