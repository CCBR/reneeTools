#' Make a heatmap
#'
#' @inheritParams filter_counts
#' @param anno_col TODO
#' @param anno_column TODO
#'
#' @return heatmap ggproto object
#' @keywords internal
#'
plot_heatmap <- function(counts_matrix, sample_metadata, sample_names_column, labels_column, anno_col, anno_column) {
  ## Annotate
  rownames(sample_metadata) <- sample_metadata[[labels_column]]
  annoVal <- lapply(anno_column, function(x) {
    out <- as.factor(sample_metadata[, x]) %>% levels()
    # names(out)=x
    return(out)
  }) %>% unlist()
  col <- anno_col[1:length(annoVal)]
  names(col) <- annoVal

  cols <- lapply(anno_column, function(x) {
    ax <- as.factor(sample_metadata[, x]) %>% levels()
    out <- col[ax]
    return(out)
  })
  names(cols) <- (anno_column)

  anno <- columnAnnotation(
    df = sample_metadata[, anno_column, drop = F],
    col = cols
  )


  ## Create Correlation Matrix

  old <- sample_metadata[[sample_names_column]]
  new <- sample_metadata[[labels_column]]
  names(old) <- new
  counts_matrix <- rename(counts_matrix, any_of(old))

  mat <- as.matrix(counts_matrix)
  tcounts <- t(mat)


  ## calculate correlation
  d <- Dist(tcounts, method = "correlation", diag = TRUE)
  m <- as.matrix(d)

  ## create dendogram
  dend <- rev(dendsort(as.dendrogram(hclust(d, method = "average"))))



  ### plot
  new.palette <- colorRampPalette(c("blue", "green", "yellow"))
  lgd <- Legend(new.palette(20), title = "Correlation", title_position = "lefttop-rot")
  hm <- Heatmap(m,
    heatmap_legend_param = list(
      title = "Correlation",
      title_position = "leftcenter-rot"
    ),
    cluster_rows = dend,
    cluster_columns = dend,
    top_annotation = anno,
    row_names_gp = gpar(fontsize = 15),
    column_names_gp = gpar(fontsize = 15),
    # heatmap_height=unit(1, "npc"),
    # heatmap_width=unit(.5, "npc"),
    # width = unit(.5, "npc"),
    # height = unit(.5, "npc"),
    col = new.palette(20)
  )

  return(hm)
}
