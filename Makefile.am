SUBDIRS = lib-src/matlisp lib-src/dfftpack lib-src/toms715 lib-src/odepack lib-src/colnew
if !EXT_BLAPACK
SUBDIRS += lib-src/blas lib-src/lapack
endif

#ACLOCAL_AMFLAGS = -I m4

#AM_FFLAGS=

F2C = @F2C@

# This should build all the libraries we need.  Then we need to
# install them before we can build the lisp code.
all : 
	(cd lib-src/matlisp; $(MAKE) install)
	(cd lib-src/dfftpack; $(MAKE) install)
	(cd lib-src/toms715; $(MAKE) install)
	(cd lib-src/odepack; $(MAKE) install)
	(cd lib-src/colnew; $(MAKE) install)
if !EXT_BLAPACK
	(cd lib-src/blas; $(MAKE) install)
	(cd lib-src/lapack; $(MAKE) install)
endif
	$(MAKE) lisp

lisp :
	$(LISPEXEC) $(LISPEVAL) '(progn (load "start.lisp"))'

clean-lisp :
	rm -rf bin/*
