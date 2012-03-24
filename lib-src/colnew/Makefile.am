lib_LTLIBRARIES = libcolnew.la

AM_FFLAGS = $(F2C)
if LIB32
AM_FFLAGS += -m32
endif

libcolnew_la_LDFLAGS = $(FLIBS_OPTS)
libcolnew_la_LIBADD = $(FLIBS)

libcolnew_la_SOURCES = \
approx.f \
appsln.f \
colnew.f \
consts.f \
contrl.f \
dgefa.f \
dgesl.f \
dmzsol.f \
errchk.f \
factrb.f \
fcblok.f \
gblock.f \
gderiv.f \
horder.f \
lsyslv.f \
newmsh.f \
rkbas.f \
sbblok.f \
shiftb.f \
skale.f \
subbak.f \
subfor.f \
vmonde.f \
vwblok.f