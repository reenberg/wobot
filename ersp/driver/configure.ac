dnl Somewhat generic plugin driver build configuration file

AC_PREREQ(2.53)

AC_INIT(libersp, 0.1)
AC_CONFIG_AUX_DIR(config)
AC_CONFIG_SRCDIR(ersp/ersp.cc)
AM_INIT_AUTOMAKE
AC_CONFIG_HEADERS(config.h)

dnl Get build and host system
AC_CANONICAL_HOST

dnl pkg-config does the wrong thing when cross-compiling.
if test "$host" != "$build"; then
	AC_MSG_ERROR([Cross-compiling is not supported])
fi

dnl ************************************************************************
dnl Check for basic tools and libraries
dnl ************************************************************************

AC_PROG_CC
AC_PROG_CXX
if test ! $GCC = yes -o ! $GXX = yes; then
	AC_MSG_ERROR([gcc and g++ is required to build])
fi
AC_PROG_CPP
AC_PROG_RANLIB
AC_PROG_LIBTOOL
AC_PROG_MAKE_SET
dnl FIXME: Require libtool

AC_CHECK_PROG(have_pkg_config,pkg-config,yes,no)
if test "x$have_pkg_config" = "xno"; then
	AC_MSG_ERROR([pkg-config is required to find local packages])
fi
m4_pattern_allow(_PKG_ERRORS)
dnl Setup some non-default pkg-config paths to easier find player etc.
export PKG_CONFIG_PATH=$PREFIX/lib/pkgconfig:/usr/local/lib/pkgconfig:$PKG_CONFIG_PATH

dnl ************************************************************************
dnl Check for Player / Stage
dnl ************************************************************************

PKG_CHECK_MODULES(PLAYERCORE, playercore, have_playercore=yes, have_playercore=no)
if test "x$have_playercore" != "xyes"; then
	AC_MSG_WARN([Check that Player is installed and that you have set PKG_CONFIG_PATH accordingly.])
	AC_MSG_ERROR([The playercore library was not found.])
fi

dnl If Stage is there it means that plugins are supported.
PKG_CHECK_MODULES(STAGE, stage, have_stage=yes, have_stage=no)
if test "x$have_stage" != "xyes"; then
	AC_MSG_WARN([The absense of stage suggests that plugin drivers are not supported.])
fi

dnl ************************************************************************
dnl Check for driver specific dependencies
dnl ************************************************************************

PKG_CHECK_MODULES(ERSP, ersp, have_ersp=yes, have_ersp=no)
if test "x$have_ersp" = "xyes"; then
	AC_DEFINE(HAVE_ERSP, 1, [ERSP library is present])
fi
AM_CONDITIONAL(HAVE_ERSP, test "x$have_ersp" = "xyes")

dnl Checks for header files.
dnl AC_CHECK_HEADERS()

dnl ************************************************************************
dnl Profile/optimization/debug build flag configuration
dnl ************************************************************************

AC_ARG_ENABLE(profile, [  --enable-profile        Compile with profiling information (with the -pg and -fprofile-arcs compile flags)],
    if test "x$enableval" = "xyes"; then
        CXXFLAGS="$CXXFLAGS -pg -fprofile-arcs"
        CFLAGS="$CFLAGS -pg -fprofile-arcs"
    fi,
)

AC_ARG_ENABLE(optimize, [  --disable-optimize      Compile without optimization (O2) ],
    if test "x$enableval" = "xyes"; then
        CXXFLAGS="$CXXFLAGS -O2"
        CFLAGS="$CFLAGS -O2"
    elif test "x$enableval" = "xno"; then
        CXXFLAGS=`echo $CXXFLAGS | sed -e s/-O.//g`
        CFLAGS=`echo $CFLAGS | sed -e s/-O.//g`
    else
        CXXFLAGS="$CXXFLAGS -$enableval"
        CFLAGS="$CFLAGS -$enableval"
    fi,
)

AC_ARG_ENABLE(debug,    [  --disable-debug         Compile without debugging information (-g)],
    if test "x$enableval" = "xyes"; then
        CXXFLAGS="$CXXFLAGS -g"
        CFLAGS="$CFLAGS -g"
    elif test "x$enableval" = "xno"; then
        CXXFLAGS=`echo $CXXFLAGS | sed -e s/-g//g`
        CFLAGS=`echo $CFLAGS | sed -e s/-g//g`
    fi,
)

dnl ************************************************************************
dnl Write out results
dnl ************************************************************************

dnl Create the driver Makefile
AC_OUTPUT(Makefile ersp/Makefile hdaps/Makefile stage/Makefile)

dnl Give configuration summary
AC_MSG_RESULT([*************************************************************])
AC_MSG_RESULT([The driver will be built using the following information:])
AC_MSG_RESULT([])
AC_MSG_RESULT([    Host  system: $host])
AC_MSG_RESULT([    C   compiler: $CC $CFLAGS])
AC_MSG_RESULT([    C++ compiler: $CXX $CXXFLAGS])
AC_MSG_RESULT([    ERSP library: $have_ersp])
AC_MSG_RESULT([])
AC_MSG_RESULT([*************************************************************])
