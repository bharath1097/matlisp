SUBDIRS = dfftpack lib-src/toms715 lib-src/odepack
if !ATLAS
SUBDIRS += LAPACK/BLAS/SRC LAPACK/SRC
endif

#ACLOCAL_AMFLAGS = -I m4

#AM_FFLAGS=

F2C = @F2C@

# This should build all the libraries we need.  Then we need to
# install them before we can build the lisp code.
all : 
	(cd dfftpack; $(MAKE) install)
	(cd lib-src/toms715; $(MAKE) install)
	(cd lib-src/odepack; $(MAKE) install)
	(cd lib-src/compat; $(MAKE) install)
if !ATLAS
	(cd LAPACK/BLAS/SRC; $(MAKE) install)
	(cd LAPACK/SRC; $(MAKE) install)
endif
	$(MAKE) lisp

lisp :
	$(LISPEXEC) $(LISPEVAL) '(progn (load "start.lisp"))'

clean-lisp :
	rm -rf bin/*