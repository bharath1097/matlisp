lib_LTLIBRARIES = libblas.la

if CMUCL
AM_FFLAGS = -m32
endif

libblas_la_LDFLAGS = $(FLIBS_OPTS)
libblas_la_LIBADD = $(FLIBS_LIBS)

if !ATLAS
libblas_la_SOURCES = \
	dasum.f \
	daxpy.f \
	dcabs1.f \
	dcopy.f \
	ddot.f \
	dgbmv.f \
	dgemm.f \
	dgemv.f \
	dger.f \
	dnrm2.f \
	drot.f \
	drotg.f \
	dsbmv.f \
	dscal.f \
	dspmv.f \
	dspr.f \
	dspr2.f \
	dswap.f \
	dsymm.f \
	dsymv.f \
	dsyr.f \
	dsyr2.f \
	dsyr2k.f \
	dsyrk.f \
	dtbmv.f \
	dtbsv.f \
	dtpmv.f \
	dtpsv.f \
	dtrmm.f \
	dtrmv.f \
	dtrsm.f \
	dtrsv.f \
	dzasum.f \
	dznrm2.f \
	icamax.f \
	idamax.f \
	isamax.f \
	izamax.f \
	lsame.f \
	xerbla.f \
	zaxpy.f \
	zcopy.f \
	zdotc.f \
	zdotu.f \
	zdscal.f \
	zgbmv.f \
	zgemm.f \
	zgemv.f \
	zgerc.f \
	zgeru.f \
	zhbmv.f \
	zhemm.f \
	zhemv.f \
	zher.f \
	zher2.f \
	zher2k.f \
	zherk.f \
	zhpmv.f \
	zhpr.f \
	zhpr2.f \
	zrotg.f \
	zscal.f \
	zswap.f \
	zsymm.f \
	zsyr2k.f \
	zsyrk.f \
	ztbmv.f \
	ztbsv.f \
	ztpmv.f \
	ztpsv.f \
	ztrmm.f \
	ztrmv.f \
	ztrsm.f \
	ztrsv.f
endif
