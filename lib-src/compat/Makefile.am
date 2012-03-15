lib_LTLIBRARIES = libmatlisp.la

AM_FFLAGS = $(F2C)
if LIB32
AM_FFLAGS += -m32
endif

libmatlisp_la_LDFLAGS =  $(FLIBS_OPTS)
libmatlisp_la_LIBADD = $(FLIBS_LIBS)

libmatlisp_la_SOURCES = \
compat.f
