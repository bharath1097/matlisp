extern int zgesv_(); extern int zgeev_(); extern int zgetrf_(); extern int zgesvd_();
extern int idamax_(); extern int dasum_(); extern int ddot_(); extern int dnrm2_();
extern int dcabs1_(); extern int dzasum_(); extern int dznrm2_(); extern int izamax_();
extern int drot_(); extern int dscal_(); extern int dswap_(); extern int dcopy_(); extern int daxpy_();
extern int zdscal_(); extern int zscal_(); extern int zswap_(); extern int zcopy_(); extern int zaxpy_(); extern int zdotc_(); extern int zdotu_();
extern int dgemv_(); extern int dsymv_(); extern int dtrmv_(); extern int dtrsv_(); extern int dger_(); extern int dsyr_(); extern int dsyr2_();
extern int zgemv_(); extern int zhemv_(); extern int ztrmv_(); extern int ztrsv_(); extern int zgerc_(); extern int zgeru_(); extern int zher2_();
extern int dgemm_(); extern int dsyrk_(); extern int dsyr2k_(); extern int dtrmm_(); extern int dtrsm_();
extern int zgemm_(); extern int ztrmm_(); extern int ztrsm_(); extern int zherk_(); extern int zher2k_();
static void __lazy_loader__(void) {
dgesv_(); dgeev_(); dgetrf_(); dgesvd_();
zgesv_(); zgeev_(); zgetrf_(); zgesvd_();
idamax_(); dasum_(); ddot_(); dnrm2_();
dcabs1_(); dzasum_(); dznrm2_(); izamax_();
drot_(); dscal_(); dswap_(); dcopy_(); daxpy_();
zdscal_(); zscal_(); zswap_(); zcopy_(); zaxpy_(); zdotc_(); zdotu_();
dgemv_(); dsymv_(); dtrmv_(); dtrsv_(); dger_(); dsyr_(); dsyr2_();
zgemv_(); zhemv_(); ztrmv_(); ztrsv_(); zgerc_(); zgeru_(); zher2_();
dgemm_(); dsyrk_(); dsyr2k_(); dtrmm_(); dtrsm_();
zgemm_(); ztrmm_(); ztrsm_(); zherk_(); zher2k_();
}
