
deg <- function(renee_ds, method, padj_set=0.05,log2fc_set=1.1 {
    if (method=="DESEQ2"){
        renee_ds@deg$deseq2 <- as.data.frame(results(dds)) %>%
            subset(padj<padj_set & abs(log2FoldChange) >log2fc_set)
    }
  return(renee_ds)
}