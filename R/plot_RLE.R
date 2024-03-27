renee_plot_rle <- function(renee_ds,counts_type) {    
    #set pheno data
    x=unique(renee_ds@sample_meta$group)
    x=as.factor(x)

    EDASeq::plotRLE(as.matrix(renee_ds@counts[,counts_type]), col=color_list[x],las=2, cex.axis = .8)
}