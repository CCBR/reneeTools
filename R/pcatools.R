renee_plot_pcatools <- function(renee_ds,column_list) {
    vst <- assay(vst(renee_ds@analyses$deseq2_ds))
    
    rna_meta_sliced=dplyr::slice(renee_ds@sample_meta$sample_id,match(as.vector(colnames(vst)), renee_ds@sample_meta$sample_id))
    
    p <- PCAtools::pca(vst, metadata = rna_meta_sliced, removeVar = 0.1)
    p_eig=PCAtools::eigencorplot(p,
        metavars = column_list)
    print(p_eig)

    renee_ds@analyses$pcatools <- p_eig$pc1
  return(renee_ds)
}