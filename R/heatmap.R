#' Make a heatmap
#'
#' @inheritParams filter_counts
#' @param anno_colors vector of colors for annotation column
#' @param anno_column annotation (group) column
#'
#' @return heatmap ggproto object
#' @keywords internal
#'
plot_heatmap <- function(counts_matrix, sample_metadata, sample_names_column, labels_column, anno_column, anno_colors) {
  ## Annotate
  rownames(sample_metadata) <- sample_metadata[[labels_column]]
  annoVal <- lapply(anno_column, function(x) {
    # TODO this only works on dataframes, not tibbles
    out <- as.factor(sample_metadata[, x]) %>% levels()
    # names(out)=x
    return(out)
  }) %>% unlist()
  col <- anno_colors[1:length(annoVal)]
  names(col) <- annoVal

  cols <- lapply(anno_column, function(x) {
    ax <- as.factor(sample_metadata[, x]) %>% levels()
    out <- col[ax]
    return(out)
  })
  names(cols) <- (anno_column)

  anno <- ComplexHeatmap::columnAnnotation(
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
  d <- amap::Dist(tcounts, method = "correlation", diag = TRUE)
  m <- as.matrix(d)

  ## create dendogram
  dend <- d %>%
    stats::hclust(method = "average") %>%
    stats::as.dendrogram() %>%
    dendsort::dendsort() %>%
    rev()

  ### plot
  new.palette <- grDevices::colorRampPalette(c("blue", "green", "yellow"))
  lgd <- ComplexHeatmap::Legend(new.palette(20), title = "Correlation", title_position = "lefttop-rot")
  hm <- ComplexHeatmap::Heatmap(m,
    heatmap_legend_param = list(
      title = "Correlation",
      title_position = "leftcenter-rot"
    ),
    cluster_rows = dend,
    cluster_columns = dend,
    top_annotation = anno,
    row_names_gp = grid::gpar(fontsize = 15),
    column_names_gp = grid::gpar(fontsize = 15),
    col = new.palette(20)
  )

  return(hm)
}
