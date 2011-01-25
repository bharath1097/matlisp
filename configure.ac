AC_INIT([matlisp], [2.0],
        [matlisp-users@lists.sourceforge.net],,
        [http://matlisp.sourceforge.net])
AM_INIT_AUTOMAKE
AC_CONFIG_MACRO_DIR([m4])
AC_PROG_CC
AC_LANG_FORTRAN77
AC_PROG_F77
AC_PROG_F77_C_O
AC_F77_LIBRARY_LDFLAGS

# Figure out what the Fortran name mangling is
# This is only for autoconf 2.49d or later!
AC_F77_FUNC(f77_name)

# Setup our environment variables based on the value of f77_name
F77_LOWER_CASE=t
F77_UNDERSCORE=t
F77_EXTRA_UNDERSCORE=nil

echo f77_name = $f77_name
case $f77_name in
  F77*)	F77_LOWER_CASE=nil
	case $f77_name in
	  F77_NAME) F77_UNDERSCORE=nil
		    ;;
 	  F77_NAME_) F77_UNDERSCORE=t
		     F77_EXTRA_UNDERSCORE=nil
		;;
 	  F77_NAME__) F77_UNDERSCORE=t
		     F77_EXTRA_UNDERSCORE=t
		;;
	esac
	;;
  f77*) F77_LOWER_CASE=t
	case $f77_name in
	  f77_name) F77_UNDERSCORE=nil
		    ;;
 	  f77_name_) F77_UNDERSCORE=t
		     F77_EXTRA_UNDERSCORE=nil
		;;
 	  f77_name__) F77_UNDERSCORE=t
		     F77_EXTRA_UNDERSCORE=t
		;;
	esac
	;;
esac

# However, ACL doesn't want our name mangler to add a trailing
# underscore because it does it by itself.
case $LISPSYS in
  acl)
    F77_UNDERSCORE=nil
    ;;
esac

enabled_lisp_count=0
# Enable cmucl
AC_ARG_ENABLE([cmucl],
AC_HELP_STRING([--enable-cmucl],
[Enable cmucl]),
[case "${enableval}" in
  yes) cmucl=true ;;
  no) cmucl=false ;;
  *) AC_MSG_ERROR([bad value ${enableval} for --enable-cmucl]) ;;
 esac
 # Don't run the user's init function!
 LISPEVAL='-noinit -eval'
 LISPEXEC=lisp
 enabled_lisp_count=`expr $enabled_lisp_count + 1`
],
[cmucl=false])
AM_CONDITIONAL([CMUCL], [test x$cmucl = xtrue])

# Enable sbcl
AC_ARG_ENABLE([sbcl],
AC_HELP_STRING([--enable-sbcl],
[Enable cmucl]),
[case "${enableval}" in
  yes) sbcl=true ;;
  no) sbcl=false ;;
  *) AC_MSG_ERROR([bad value ${enableval} for --enable-sbcl]) ;;
 esac
 # Don't run the user's init function!
 LISPEVAL='--sysinit /dev/null --userinit /dev/null --eval'
 LISPEXEC=sbcl
 enabled_lisp_count=`expr $enabled_lisp_count + 1`
],
[sbcl=false])

# Enable acl
AC_ARG_ENABLE([acl],
AC_HELP_STRING([--enable-acl],
[Enable acl]),
[case "${enableval}" in
  yes) acl=true ;;
  no) acl=false ;;
  *) AC_MSG_ERROR([bad value ${enableval} for --enable-acl]) ;;
 esac
 LISPEVAL='-e'
 LISPEXEC=cl
 enabled_lisp_count=`expr $enabled_lisp_count + 1`
],
[acl=false])

# Only one lisp can be enabled at a time.
if test $enabled_lisp_count -gt 1; then
  AC_MSG_ERROR([Only one Lisp can be enabled at a time.])
fi

# Allow user to specify the Lisp executable name.  (The names aren't
# standardized, and might actually conflict.)

AC_ARG_WITH([lisp-exec],
AC_HELP_STRING([--with-lisp-exec=name],[Name of lisp program]),
[LISPEXEC=$withval]
)


AC_PROG_LIBTOOL

# Extract the library part and options part out of FLIBS.
FLIBS_LIBS=`echo $FLIBS | tr -s ' ' '\n' | grep -e -l | grep -v gfortranbegin | tr -s '\n' ' '`
FLIBS_OPTS=`echo $FLIBS | tr -s ' ' '\n' | grep -v gfortranbegin | grep -v x86_64 | grep -e -L | tr -s '\n' ' '`

echo FLIBS_LIBS = $FLIBS_LIBS
echo FLIBS-OPTS = $FLIBS_OPTS

AC_SUBST(LISPEXEC)
AC_SUBST(BLAS_OBJS)
AC_SUBST(NO_ATLAS_LAPACK_OBJS)
AC_SUBST(ATLAS_DIR)
AC_SUBST(ATLAS_LIBS)
AC_SUBST(ATLAS_P)
AC_SUBST(F77_LOWER_CASE)
AC_SUBST(F77_UNDERSCORE)
AC_SUBST(F77_EXTRA_UNDERSCORE)
AC_SUBST(LISPEXEC)
AC_SUBST(LISPEVAL)
AC_SUBST(srcdir)
AC_SUBST(FLIBS_LIBS)
AC_SUBST(FLIBS_OPTS)
AC_SUBST(share_ext)

AM_FFLAGS=

echo host = $host
case $host in
  *darwin*) share_ext=dylib ;;
  *) share_ext=so ;;
esac

AC_CONFIG_FILES([
  matlisp.mk
  Makefile
  start.lisp
  config.lisp
  lib/lazy-loader.lisp
  LAPACK/SRC/Makefile
  LAPACK/BLAS/SRC/Makefile
  dfftpack/Makefile
  lib-src/toms715/Makefile
  src/f77-mangling.lisp
])

# Allow user to use ATLAS if available.
# We assume the standard names for the ATLAS libraries.
AC_ARG_WITH([atlas],
AC_HELP_STRING([--with-atlas=libpath], [Location of the ATLAS libraries]),
[
  # Building with ATLAS
  ATLAS_DIR="-L$withval"
  ATLAS_LIBS="-llapack -lcblas -lf77blas -latlas"
  ATLAS_P=t
],
[
  # Building without ATLAS.  Need these objects from our own copy of 
  # LAPACK.
  ATLAS_P=nil
BLAS_OBJS="dgemm.o dswap.o dtrmv.o lsame.o zdotu.o zhemv.o ztrmv.o dgemv.o  dsymv.o  dtrsm.o  zher2.o   ztrsm.o dasum.o  dger.o   dsyr.o   dtrsv.o   zdscal.o zher2k.o ztrsv.o daxpy.o  dsyr2.o  dzasum.o xerbla.o  zgemm.o  zherk.o dcabs1.o dnrm2.o  dsyr2k.o dznrm2.o  zaxpy.o  zgemv.o  zscal.o dcopy.o  drot.o   dsyrk.o  idamax.o  zcopy.o  zgerc.o  zswap.o ddot.o	 dscal.o  dtrmm.o  izamax.o  zdotc.o  zgeru.o  ztrmm.o dsymm.o"
NO_ATLAS_LAPACK_OBJS="dgetrf.o zgetrf.o dgetrs.o zgetrs.o dlaswp.o zlaswp.o dgesv.o zgesv.o"
])

AC_OUTPUT