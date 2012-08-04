lib_LTLIBRARIES = libscaldiv.la

AM_FFLAGS = $(F2C)
if LIB32
AM_FFLAGS += -m32
endif

libscaldiv_la_LDFLAGS = $(FLIBS_OPTS)
libscaldiv_la_LIBADD = $(FLIBS)

libscaldiv_la_SOURCES = \
descal.f\
zescal.f\
dediv.f\
zediv.f
