lib_LTLIBRARIES = liblapack.la

AM_FFLAGS = -ff2c
if LIB32
AM_FFLAGS += -m32
endif

liblapack_la_LIBADD = ../BLAS/SRC/libblas.la $(FLIBS)
liblapack_la_LDFLAGS = $(FLIBS_OPTS) -version-info 3:3:0

if !ATLAS
liblapack_la_SOURCES = \
	dlasq5.f dlasq6.f ieeeck.f zdrot.f dlabad.f \
	dlarf.f   dorglq.f  zhetd2.f  zlatrs.f \
	dbdsqr.f  dlabrd.f  dlarfb.f  dorgql.f	 zhetrd.f  zpotf2.f \
	dgebak.f  dlacon.f  dlarfg.f  dorgqr.f	 zhseqr.f  zpotrf.f \
	dgebal.f  dlacpy.f  dlarft.f  dorgtr.f	 zbdsqr.f  zlabrd.f  zrot.f \
	dgebd2.f  dladiv.f  dlarfx.f  dorm2r.f	 zdrscl.f  zlacgv.f  zsteqr.f \
	dgebrd.f  dlae2.f   dlartg.f  dormbr.f	 zgebak.f  zlacon.f  ztrevc.f \
	dgeesx.f  dlaev2.f  dlas2.f   dorml2.f	 zgebal.f  zlacpy.f  ztrexc.f \
	dgeev.f	  dlaexc.f  dlascl.f  dormlq.f	 zgebd2.f  zladiv.f  ztrsen.f \
	dgehd2.f  dlag2.f   dlaset.f  dormqr.f	 zgebrd.f  zlahqr.f  ztrsyl.f \
	dgehrd.f  dlahqr.f  dlasq1.f  drscl.f	 zgeesx.f  zlahrd.f  zung2l.f \
	dgelq2.f  dlahrd.f  dlasq2.f  dsteqr.f	 zgeev.f   zlange.f  zung2r.f \
	dgelqf.f  dlaln2.f  dlasq3.f  dsterf.f	 zgehd2.f  zlanhe.f  zungbr.f \
	dgelss.f  dlasq4.f  dsyev.f   zgehrd.f   zlanhs.f  zunghr.f \
	dgeqp3.f  dlaqp2.f  dlaqps.f  zgeqp3.f   zlaqp2.f  zlaqps.f \
	dgeqpf.f  dlasr.f   dsytd2.f  zgelq2.f   zlarf.f   zungl2.f \
	dgeqr2.f  dlasrt.f  dsytrd.f  zgelqf.f   zlarfb.f  zunglq.f \
	dgeqrf.f  dlassq.f  dtrevc.f  zgelss.f   zlarfg.f  zungql.f \
	dlasv2.f  dtrexc.f  zgeqpf.f   zlarft.f  zungqr.f \
	dgesvd.f  dlamch.f  dtrsen.f	 zgeqr2.f  zlarfx.f  zungtr.f \
	dgetf2.f  dlange.f  dlasy2.f  dtrsyl.f	 zgeqrf.f  zlartg.f  zunm2r.f \
	dlanhs.f  dlatrd.f  dzsum1.f  zlascl.f  zunmbr.f \
	dlanst.f  dorg2l.f  \
	ilaenv.f iladlc.f iladlr.f ilazlc.f ilazlr.f iparmq.f \
	zgesvd.f  zlaset.f  zunml2.f \
	dggbal.f  dlansy.f  dorg2r.f  izmax1.f	 zgetf2.f  zlasr.f   zunmlq.f \
	dgghrd.f  dlanv2.f  dorgbr.f  zlassq.f  zunmqr.f \
	dhgeqz.f  dlapy2.f  dorghr.f  \
	dhseqr.f  dlapy3.f  dorgl2.f  zheev.f    zlatrd.f \
	dpotrf.f  dpotf2.f  dpotrs.f  zpotrs.f \
	dgesv.f dgetrf.f dgetrs.f dlaswp.f zgetrs.f zlaswp.f zgesv.f \
	zgetrf.f \
	dlaic1.f dlatrz.f dlarz.f dlarzb.f dlarzt.f dgelsy.f dtzrzf.f \
	dormr3.f dormrz.f \
	disnan.f dlacn2.f dlahr2.f dlaisnan.f \
	dlaqr0.f dlaqr1.f dlaqr2.f dlaqr3.f dlaqr4.f dlaqr5.f \
	zlaqr0.f zlaqr1.f zlaqr2.f zlaqr3.f zlaqr4.f zlaqr5.f \
	dormhr.f \
	zlacn2.f zlahr2.f zunmhr.f 
endif
