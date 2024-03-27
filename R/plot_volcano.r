plot_volcano<-function(renee_ds,padj_set=0.05,log2fc_set=1.1){
    
  # create results df
  res_df=as.data.frame(results(renee_ds@analysis$deseq2))
  res_df$gene_id=rownames(res_df)
  
  #merge with counts 
  annotated_df=merge(res_df,renee_ds@counts$filtered_counts[,c("gene_id","GeneName")])
  annotated_df=annotated_df[complete.cases(annotated_df), ]

  # add a column of diffexpressed
  annotated_df$diffexpressed <- "NO"
  annotated_df$diffexpressed=ifelse(annotated_df$log2FoldChange > log2fc_set & annotated_df$pvalue < padj_set, "UP", annotated_df$diffexpressed)
  annotated_df$diffexpressed=ifelse(annotated_df$log2FoldChange < -log2fc_set & annotated_df$pvalue < padj_set, "DOWN", annotated_df$diffexpressed)

  # add gene label
  annotated_df$uselabel=""
  annotated_df$uselabel=ifelse(annotated_df$diffexpressed=="UP" | annotated_df$diffexpressed == "DOWN",annotated_df$GeneName,annotated_df$uselabel)

  p <- ggplot(data=annotated_df, 
              aes(x=log2FoldChange,
                  y=-log10(pvalue), 
                  col=diffexpressed,
                  label=uselabel)) + 
    geom_point() + 
    theme_minimal() +
    geom_text_repel() + 
    scale_color_manual(values=c("blue", "grey", "red"))+
    geom_vline(xintercept=c(-log2fc_set, log2fc_set), col="red") +
    geom_hline(yintercept=-log10(padj_set), col="red")
  print(p)
}