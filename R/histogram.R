plot_histogram <- function(log_counts,
                           sample_metadata,
                           gene_names_column,
                           labels_column,
                           color_histogram_by_group = FALSE,
                           set_min_max_for_x_axis_for_histogram = FALSE,
                           minimum_for_x_axis_for_histogram = -1,
                           maximum_for_x_axis_for_histogram = 1,
                           legend_position_for_histogram = "top",
                           legend_font_size_for_histogram = 10,
                           number_of_histogram_legend_columns = 6) {
  df.m <- melt(log_counts, id.vars = c(gene_names_column))
  df.m <- dplyr::rename(df.m, sample = Var2)

  if (set_min_max_for_x_axis_for_histogram == TRUE) {
    xmin <- minimum_for_x_axis_for_histogram
    xmax <- maximum_for_x_axis_for_histogram
  } else {
    xmin <- min(df.m$value)
    xmax <- max(df.m$value)
  }

  if (color_histogram_by_group == TRUE) {
    df.m %>% mutate(colgroup = sample_metadata[sample, groups_column]) -> df.m
    df.m <- df.m[complete.cases(df.m[, "colgroup"]), ]
    df.m$colgroup <- gsub("\\s", "_", df.m$colgroup)
    df.m$colgroup <- factor(df.m$colgroup, levels = unique(df.m$colgroup))
    ## print(unique(df.m$sample))
    n <- length(levels(df.m$colgroup))
    cols <- colorval[1:n]

    # plot Density
    histPlot <- ggplot(df.m, aes(x = value, group = sample)) +
      geom_density(aes(colour = colgroup), size = 1)
  } else {
    df.m$sample <- sample_metadata[df.m$sample, labels_column]
    n <- length(unique(df.m$sample))
    cols <- getourrandomcolors(n)

    histPlot <- ggplot(df.m, aes(x = value, group = sample)) +
      geom_density(aes(colour = sample), size = 1)
  }

  histPlot <- histPlot +
    xlab("Normalized Counts") + ylab("Density") +
    theme_bw() +
    theme(
      legend.position = legend_position_for_histogram,
      legend.text = element_text(size = legend_font_size_for_histogram),
      legend.title = element_blank(),
      panel.background = element_blank(),
      axis.text = element_text(size = 18),
      axis.title = element_text(size = 20),
      panel.border = element_rect(colour = "black", fill = NA, size = 0),
      axis.line = element_line(size = .5),
      axis.ticks = element_line(size = 1)
    ) +
    # ggtitle("Frequency Histogram") +
    xlim(xmin, xmax) +
    # scale_linetype_manual(values=rep(c('solid', 'dashed','dotted','twodash'),n)) +
    scale_colour_manual(values = cols) +
    guides(linetype = guide_legend(ncol = number_of_histogram_legend_columns))

  return(histPlot)
}
