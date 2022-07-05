(define-module (r0man home packages)
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
  #:use-module (gnu packages gimp)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages inkscape)
  #:use-module (gnu packages java)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages man)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages uml)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages)
  #:export (packages))

(define packages
  (list (list openjdk11 "jdk")
        arandr
        cmake
        dbus
        elixir
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
        gnuplot
        gnutls
        graphviz
        guile-3.0
        guile-git
        guile-json-1
        guile-lib
        guile-lzlib
        guile-sqlite3
        guile-zlib
        help2man
        inkscape
        isync
        lapack
        libatasmart
        libffi
        libgcrypt
        libvterm
        mu
        nss-certs
        openblas
        openblas-ilp64
        pkg-config
        plantuml
        redshift
        rofi
        rxvt-unicode
        sed
        shared-mime-info
        sqitch
        sqlite
        strace
        xdg-utils
        xrandr
        xrdb
        xscreensaver
        zip))
