SUBDIRS = LAPACK/BLAS/SRC LAPACK/SRC dfftpack lib-src/toms715
#ACLOCAL_AMFLAGS = -I m4

#AM_FFLAGS=

# This should build all the libraries we need.  Then we need to
# install them before we can build the lisp code.
all : 
	$(MAKE) install
	$(MAKE) lisp

lisp :
	$(LISPEXEC) $(LISPEVAL) '(progn (load "start.lisp"))'

clean-lisp :
	rm -rf bin/*