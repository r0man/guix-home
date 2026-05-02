dnl -*- Autoconf -*-

AC_INIT([guile-r0man-home], [0.1])
AC_CONFIG_SRCDIR([modules/r0man/guix/channels.scm])
AC_CONFIG_AUX_DIR([build-aux])
AC_REQUIRE_AUX_FILE([test-driver.scm])
AM_INIT_AUTOMAKE([1.12 gnu silent-rules subdir-objects color-tests parallel-tests -Woverride -Wno-portability])
AM_SILENT_RULES([yes])

AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([pre-inst-env], [chmod +x pre-inst-env])

AC_PROG_SED

dnl Search for 'guile' and 'guild'.  This macro defines
dnl 'GUILE_EFFECTIVE_VERSION'.
GUILE_PKG([3.0])
GUILE_PROGS
GUILE_SITE_DIR
if test "x$GUILD" = "x"; then
   AC_MSG_ERROR(['guild' binary not found; please check your guile-3.x installation.])
fi

dnl Installation directories for .scm and .go files.
guilemoduledir="${datarootdir}/guile/site/$GUILE_EFFECTIVE_VERSION"
guileobjectdir="${libdir}/guile/$GUILE_EFFECTIVE_VERSION/site-ccache"
AC_SUBST([guilemoduledir])
AC_SUBST([guileobjectdir])

AC_OUTPUT
