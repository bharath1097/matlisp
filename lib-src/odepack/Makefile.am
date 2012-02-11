lib_LTLIBRARIES = libodepack.la

AM_FFLAGS = -ff2c
if CMUCL
AM_FFLAGS += -m32
endif

libodepack_la_LDFLAGS = $(FLIBS_OPTS)
libodepack_la_LIBADD = $(FLIBS)

libodepack_la_SOURCES = \
adjlr.f \
cdrv.f \
cntnzu.f \
daigbt.f \
dainvg.f \
dainvgs.f \
datp.f \
datv.f \
daxpy.f \
dbnorm.f \
dcfode.f \
dcopy.f \
ddecbt.f \
ddot.f \
dewset.f \
dfnorm.f \
dgbfa.f \
dgbsl.f \
dgefa.f \
dgesl.f \
dhefa.f \
dhels.f \
dheqr.f \
dhesl.f \
dintdy.f \
diprep.f \
diprepi.f \
dlhin.f \
dlsoda.f \
dlsodar.f \
dlsode.f \
dlsodes.f \
dlsodi.f \
dlsodis.f \
dlsodkr.f \
dlsodpk.f \
dlsoibt.f \
dmnorm.f \
dnrm2.f \
dorthog.f \
dpcg.f \
dpcgs.f \
dpjibt.f \
dpkset.f \
dprep.f \
dprepi.f \
dprepj.f \
dprepji.f \
dprja.f \
dprjis.f \
dprjs.f \
drchek.f \
droots.f \
dscal.f \
dsetpk.f \
dslsbt.f \
dsolbt.f \
dsolpk.f \
dsolss.f \
dsolsy.f \
dspigmr.f \
dspiom.f \
dsrcar.f \
dsrckr.f \
dsrcma.f \
dsrcms.f \
dsrcom.f \
dsrcpk.f \
dstoda.f \
dstode.f \
dstodi.f \
dstodpk.f \
dstoka.f \
dumach.f \
dumsum.f \
dusol.f \
dvnorm.f \
idamax.f \
iumach.f \
ixsav.f \
jgroup.f \
md.f \
mdi.f \
mdm.f \
mdp.f \
mdu.f \
nnfc.f \
nnsc.f \
nntc.f \
nroc.f \
nsfc.f \
odrv.f \
sro.f \
xerrwd.f \
xsetf.f \
xsetun.f
