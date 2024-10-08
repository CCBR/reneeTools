#' Perform and plot a Principal Components Analysis
#'
#' @param log_counts log-transformed filtered counts
#' @inheritParams filter_counts
#' @param samples_to_include samples in `sample_metadata` to include in the analysis
#'
#' @return PCA plot
#' @keywords internal
#'
plot_pca <- function(log_counts,
                     sample_metadata,
                     samples_to_include,
                     samples_to_rename_manually,
                     group_column,
                     label_column,
                     color_values,
                     principal_component_on_x_axis = 1,
                     principal_component_on_y_axis = 2,
                     legend_position_for_pca = "top",
                     point_size_for_pca = 1,
                     add_label_to_pca = TRUE,
                     label_font_size = 3,
                     label_offset_y_ = 2,
                     label_offset_x_ = 2) {
  # calculate PCA
  var <- xdata <- ydata <- group <- NULL
  tedf <- t(log_counts)
  tedf <- tedf[, colSums(is.na(tedf)) != nrow(tedf)]
  tedf <- tedf[, apply(tedf, 2, var) != 0]
  pca <- stats::prcomp(tedf, scale. = T)

  pcx <- paste0("PC", principal_component_on_x_axis)
  pcy <- paste0("PC", principal_component_on_y_axis)
  pca.df <- as.data.frame(pca$x) %>% dplyr::select(tidyselect::all_of(c(pcx, pcy)))

  pca.df$group <- sample_metadata[[group_column]]
  pca.df$sample <- sample_metadata[[label_column]]
  perc.var <- (pca$sdev^2 / sum(pca$sdev^2)) * 100
  perc.var <- formatC(perc.var, format = "g", digits = 4)
  pc.x.lab <- paste0(pcx, " ", perc.var[principal_component_on_x_axis], "%")
  pc.y.lab <- paste0(pcy, " ", perc.var[principal_component_on_y_axis], "%")
  labelpos <- pca.df
  labelpos$mean_y <- pca.df[[pcy]] + label_offset_y_
  labelpos$mean_x <- pca.df[[pcx]] + label_offset_x_
  pca.df$xdata <- pca.df[[pcx]]
  pca.df$ydata <- pca.df[[pcy]]

  # rename samples
  pca.df <- rename_samples(pca.df, samples_to_rename_manually)

  # plot PCA
  pcaPlot <- pca.df %>%
    ggplot2::ggplot(ggplot2::aes(x = xdata, y = ydata, text = sample)) +
    ggplot2::geom_point(ggplot2::aes(color = group),
      size = point_size_for_pca
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = legend_position_for_pca,
      legend.title = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(size = 18),
      axis.title = ggplot2::element_text(size = 20),
      panel.border = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 1),
      axis.ticks = ggplot2::element_line(linewidth = 1),
      legend.text = ggplot2::element_text(size = 18)
    ) +
    ggplot2::coord_fixed(ratio = 1.5) +
    ggplot2::scale_colour_manual(values = color_values) +
    ggplot2::xlab(pc.x.lab) +
    ggplot2::ylab(pc.y.lab)

  if (add_label_to_pca == TRUE) {
    pcaPlot <- pcaPlot +
      ggrepel::geom_text_repel(ggplot2::aes(label = sample, color = group),
        size = 7,
        show.legend = F,
        direction = c("both"),
        box.padding = 1.25
      )
  }
  return(pcaPlot)
}
