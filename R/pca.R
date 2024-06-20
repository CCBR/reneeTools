#' Perform and plot a Principal Coordinate Analysis
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
                     groups_column,
                     labels_column,
                     color_values,
                     principal_component_on_x_axis = 1,
                     principal_component_on_y_axis = 2,
                     legend_position_for_pca = "top",
                     point_size_for_pca = 1,
                     add_labels_to_pca = TRUE,
                     label_font_size = 3,
                     label_offset_y_ = 2,
                     label_offset_x_ = 2) {
  # calculate PCA
  tedf <- t(log_counts)
  tedf <- tedf[, colSums(is.na(tedf)) != nrow(tedf)]
  tedf <- tedf[, apply(tedf, 2, var) != 0]
  pca <- prcomp(tedf, scale. = T)

  pcx <- paste0("PC", principal_component_on_x_axis)
  pcy <- paste0("PC", principal_component_on_y_axis)
  pca.df <- as.data.frame(pca$x) %>% dplyr::select(.data[[pcx]], .data[[pcy]])

  pca.df$group <- sample_metadata[[groups_column]]
  pca.df$sample <- sample_metadata[[labels_column]]
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
  pcaPlot <- ggplot(pca.df, aes(x = xdata, y = ydata, text = sample)) +
    geom_point(aes(color = group), text = sample, size = point_size_for_pca) +
    theme_bw() +
    theme(
      legend.position = legend_position_for_pca,
      legend.title = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.text = element_text(size = 18),
      axis.title = element_text(size = 20),
      # panel.grid.major = element_line(size = 1),
      # axis.line=element_line(size=1),
      panel.border = element_rect(colour = "black", fill = NA, size = 1),
      axis.ticks = element_line(size = 1),
      legend.text = element_text(size = 18)
    ) +
    ggplot2::coord_fixed(ratio = 1.5) +
    scale_colour_manual(values = color_values) +
    xlab(pc.x.lab) +
    ylab(pc.y.lab)

  if (add_labels_to_pca == TRUE) {
    pcaPlot <- pcaPlot +
      geom_text_repel(aes(label = sample, color = group),
        size = 7,
        show.legend = F,
        direction = c("both"),
        box.padding = 1.25
      )
  }
  return(pcaPlot)
}
