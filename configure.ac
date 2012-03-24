dnl Copyright (c) 2000 The Regents of the University of California.
dnl All rights reserved. 
dnl
dnl Permission is hereby granted, without written agreement and without
dnl license or royalty fees, to use, copy, modify, and distribute this
dnl software and its documentation for any purpose, provided that the
dnl above copyright notice and the following two paragraphs appear in all
dnl copies of this software.
dnl
dnl IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY
dnl FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES
dnl ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF
dnl THE UNIVERSITY OF CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF
dnl SUCH DAMAGE.
dnl
dnl THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
dnl INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
dnl MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE. THE SOFTWARE
dnl PROVIDED HEREUNDER IS ON AN "AS IS" BASIS, AND THE UNIVERSITY OF
dnl CALIFORNIA HAS NO OBLIGATION TO PROVIDE MAINTENANCE, SUPPORT, UPDATES,
dnl ENHANCEMENTS, OR MODIFICATIONS.
dnl
dnl ======================================================================
dnl
dnl Process this file with autoreconf to produce the appropriate configure
dnl script and Makefile.in's.  Then run configure to build.
dnl

AC_INIT([matlisp], [2.9],
        [matlisp-users@lists.sourceforge.net],,
        [http://matlisp.sourceforge.net])
AM_INIT_AUTOMAKE
AC_CONFIG_MACRO_DIR([m4])

dnl Get a nice grep command.
AC_PROG_GREP

dnl Determine which C compiler to use.  (Probably don't need this
dnl because we no longer use a C compiler to compile anything.)
AC_PROG_CC

dnl Set the Fortran language.
AC_LANG_FORTRAN77

dnl Determine a Fortran 77 compiler to use.  If `F77' is not already
dnl set in the environment, check for `g77', `f77' and `f2c', in that
dnl order.  Set the output variable `F77' to the name of the compiler
dnl found.
AC_PROG_F77

dnl We need -c and -o supported by the Fortran compiler
AC_PROG_F77_C_O

dnl Figure out what libraries we need to link with the Fortran 
dnl compiler
AC_F77_LIBRARY_LDFLAGS

# Figure out what the Fortran name mangling is
# This is only for autoconf 2.49d or later!
AC_F77_FUNC(f77_name)

# Setup our environment variables based on the value of f77_name.  For
# the record, F77_EXTRA_UNDERSCORE means that any embedded underscores
# in the Fortran name are converted to two underscores for the C
# equivalent name.
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
 LISPQUIT='(quit)'
 enabled_lisp_count=`expr $enabled_lisp_count + 1`
],
[cmucl=false])
AM_CONDITIONAL([CMUCL], [test x$cmucl = xtrue])
dnl AM_CONDITIONAL([LIB32], [test x$cmucl = xtrue])

# Enable sbcl
AC_ARG_ENABLE([sbcl],
AC_HELP_STRING([--enable-sbcl],
[Enable sbcl]),
[case "${enableval}" in
  yes) sbcl=true ;;
  no) sbcl=false ;;
  *) AC_MSG_ERROR([bad value ${enableval} for --enable-sbcl]) ;;
 esac
 # Don't run the user's init function!
 LISPEVAL='--sysinit /dev/null --userinit /dev/null --eval'
 LISPEXEC=sbcl
 LISPQUIT='(sb-ext:quit)'
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

# Enable ccl
AC_ARG_ENABLE([ccl],
AC_HELP_STRING([--enable-ccl],
[Enable ccl]),
[case "${enableval}" in
  yes) ccl=true ;;
  no) ccl=fasle ;;
  *) AC_MSG_ERROR([bad value ${enableval} for --enable-ccl]) ;;
 esac
 LISPEVAL='-e'
 LISPEXEC=ccl
 LISPQUIT='(quit)'
 enabled_lisp_count=`expr $enabled_lisp_count + 1`
],
[ccl=false])
AM_CONDITIONAL([CCL], [test x$ccl = xtrue])

# Enable ecl
AC_ARG_ENABLE([ecl],
AC_HELP_STRING([--enable-ecl],
[Enable ecl]),
[case "${enableval}" in
  yes) ecl=true ;;
  no) ecl=fasle ;;
  *) AC_MSG_ERROR([bad value ${enableval} for --enable-ecl]) ;;
 esac
 LISPEVAL='-eval'
 LISPEXEC=ecl
 enabled_lisp_count=`expr $enabled_lisp_count + 1`
],
[ecl=false])
AM_CONDITIONAL([ECL], [test x$ecl = xtrue])

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

dnl We need libtool to build the libraries
AC_PROG_LIBTOOL

# Extract the library part and options part out of FLIBS.
FLIBS_LIBS=`echo $FLIBS | tr -s ' ' '\n' | $GREP -e -l | $GREP -v gfortranbegin | tr -s '\n' ' '`
FLIBS_OPTS=`echo $FLIBS | tr -s ' ' '\n' | $GREP -v gfortranbegin | $GREP -v x86_64 | $GREP -e -L | tr -s '\n' ' '`

GIT_VERSION=`git describe --dirty`

# Check that quicklisp exists.  We need that currently to get cffi if
# it's not already available.
AC_CHECK_FILE($HOME/quicklisp/setup.lisp, HAVE_QL="(and)", HAVE_QL="(or)")

# Are we using a 32-bit lisp?  If so, define LIB32 so that we compile
# the 32-bit libraries.

AC_MSG_CHECKING([for 32-bit lisp])
AM_CONDITIONAL([LIB32], 
  $LISPEXEC $LISPEVAL "(progn (format t \"~S~%\" (> most-positive-fixnum (ash 1 32))) $LISPQUIT)" | grep '^NIL' >/dev/null)
if test -z "${LIB32_TRUE}" ; then
  AC_MSG_RESULT([yes])
else
  AC_MSG_RESULT([no])
fi

dnl # Check to see if gfortran needs -ff2c.  If needed, this option forces
dnl # gfortran to be compatible with f2c.  This is needed by matlisp's FFI
dnl # so that it can correctly handle Fortran functions that return
dnl # complex numbers.  With f2c, complex numbers cause a hidden initial
dnl # parameter that is a pointer to a structure where the complex result
dnl # is stored.

dnl if test x"$F77" = x"gfortran"; then
dnl   AC_MSG_CHECKING([if gfortran needs -ff2c for functions returning complex numbers])
dnl   # Just some silly Fortran function that returns a complex number.
dnl   cat > conftest.f <<EOF
dnl       complex*16 function f77_name(x)
dnl       complex*16 x
dnl       f77_name = 3*x
dnl       return
dnl       end
dnl EOF
dnl   #echo $F77 $FFLAGS -c conftest.f
dnl   $F77 $FFLAGS -c conftest.f
dnl   # A C main routine that calls the fortran routine.  This assumes the
dnl   # f2c calling convention of a hidden result structure.  If the real
dnl   # part of the result matches our expectations, then main returns 0
dnl   # (f2c compatible).  Any other exit code means incompatible.
dnl   cat > ctest.c <<EOF
dnl #include <stdio.h>
dnl #include <math.h>
dnl 
dnl struct c16 
dnl {
dnl   double real;
dnl   double imag;
dnl };
dnl 
dnl   
dnl extern void ${f77_name}(struct c16* result, struct c16* x);
dnl 
dnl int main()
dnl {
dnl   int rc;
dnl   
dnl   struct c16 x, y;
dnl   
dnl   x.real = 1;
dnl   x.imag = 2;
dnl 
dnl   ${f77_name}(&y, &x);
dnl 
dnl   if (fabs(y.real - 3) < 1e-5) {
dnl     rc = 0;
dnl   } else {
dnl     rc = 1;
dnl   }
dnl 
dnl   return rc;
dnl }
dnl EOF
dnl   #echo $CC $CFLAGS -o a.out ctest.c conftest.o $LIBS
dnl   $CC $CFLAGS -o a.out ctest.c conftest.o $LIBS
dnl   NEED_FF2C=yes
dnl   F2C="-ff2c"
dnl   if a.out; then
dnl     NEED_FF2c=no
dnl     F2C=""
dnl   fi
dnl   rm -f ctest.c a.out
dnl   AC_MSG_RESULT([$NEED_FF2C])
dnl fi
dnl 
# Allow user to use ATLAS if available.
# We assume the standard names for the ATLAS libraries.
AC_ARG_WITH([atlas],
AC_HELP_STRING([--with-atlas=libpath], [Location of the ATLAS libraries]),
[
  # Building with ATLAS
  ATLAS_DIR="$withval/"
  ATLAS_LIBS="libatlas libcblas libf77blas liblapack"
  atlas=true
  ATLAS_P=t
],
[
  # Building without ATLAS.  Need these objects from our own copy of 
  # LAPACK.
  atlas=false
  ATLAS_P=nil
])
AM_CONDITIONAL([ATLAS], [test x$atlas = xtrue])

# Check to see if the ATLAS libraries are compatible with matlisp's
# ffi.  Basically the same test as above that checks to see if -ff2c
# is needed.  We call zdotu which is a Fortran function returning a
# complex number.  Matlisp assumes such functions return the result by
# storing the answer at address given by a hidden first parameter to
# the function.

if test x"$atlas" = xtrue; then
  AC_MSG_CHECKING([if ATLAS is compatible with f2c calling conventions])
  # From the value of f77_name, figure out the actual name for
  # Fortran's zdotu.
  case $f77_name in
    F77*) case $f77_name in
	    F77_NAME) ZDOTU="ZDOTU" ;;
	    F77_NAME_) ZDOTU="ZDOTU_" ;;
	    F77_NAME__) ZDOTU="ZDOTU_" ;;
	  esac
	  ;;
    f77*) case $f77_name in
	    f77_name) ZDOTU="zdotu" ;;
	    f77_name_) ZDOTU="zdotu_" ;;
	    f77_name__) ZDOTU="zdotu_" ;;
	  esac
	  ;;
  esac

  cat > conftest.c <<EOF
[
#include <stdio.h>
#include <math.h>
  
extern void ${ZDOTU}(double *, int *, double *, int *, double *, int *);

int main()
{
  int rc;
  
  int n = 2;
  int incx = 1;
  double x[4];
  double out[2];
  x[0] = 1;
  x[1] = 0;
  x[2] = 2;
  x[3] = 0;
  out[0] = 0;
  out[1] = 0;

  ${ZDOTU}(out, &n, x, &incx, x, &incx);

  if (fabs(out[0] - 5) < 1e-10) {
    rc = 0;
  } else {
    printf("Actual output = %lg, %lg, instead of 5, 0\n", out[0], out[1]);
    rc = 1;
  }

  return rc;
}
]
EOF
  $CC $CFLAGS -c conftest.c
  $F77 $FFLAGS -o a.out conftest.o -L${ATLAS_DIR} -latlas -lcblas -lf77blas -llapack
  if a.out; then
    AC_MSG_RESULT([yes])
    F2C=-ff2c
  else
    AC_MSG_RESULT([no])
    F2C=""
  fi
fi

# The following variables will be substituted into the .in files
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
AC_SUBST(libdir)
AC_SUBST(FLIBS_LIBS)
AC_SUBST(FLIBS_OPTS)
AC_SUBST(share_ext)
AC_SUBST(GIT_VERSION)
AC_SUBST(HAVE_QL)
AC_SUBST(F2C)

echo host = $host

# Set the extension for shared libraries.  This is not very robust.
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
  src/f77-mangling.lisp
  LAPACK/SRC/Makefile
  LAPACK/BLAS/SRC/Makefile
  dfftpack/Makefile
  lib-src/toms715/Makefile
  lib-src/compat/Makefile
  lib-src/odepack/Makefile
  lib-src/colnew/Makefile
])

echo FLIBS = $FLIBS
echo HAVE_QL = $HAVE_QL
AC_OUTPUT
