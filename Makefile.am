SUBDIRS = lib/matlisp lib/dfftpack lib/toms715 lib/odepack lib/colnew
if !EXT_BLAPACK
SUBDIRS += lib/blas lib/lapack
endif

ACLOCAL_AMFLAGS = -I m4

#AM_FFLAGS=

F2C = @F2C@

# This should build all the libraries we need.  Then we need to
# install them before we can build the lisp code.
all : 
	(cd lib/matlisp; $(MAKE) install)
	(cd lib/dfftpack; $(MAKE) install)
	(cd lib/toms715; $(MAKE) install)
	(cd lib/odepack; $(MAKE) install)
	(cd lib/colnew; $(MAKE) install)
if !EXT_BLAPACK
	(cd lib/blas; $(MAKE) install)
	(cd lib/lapack; $(MAKE) install)
endif
	$(MAKE) lisp

lisp :
	$(LISPEXEC) $(LISPEVAL) '(progn (load "start.lisp"))'

clean-lisp :
	rm -rf bin/*
