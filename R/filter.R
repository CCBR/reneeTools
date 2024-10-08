#' Filter low counts
#'
#' This is often the first step in the QC portion of an analysis to filter out
#' features that have very low raw counts across most or all of your samples.
#'
#' This function takes a reneeDataSet containing raw counts and a sample
#' metadata table, and returns the reneeDataSet object with filtered counts.
#' It also produces an image consisting of three QC plots.
#'
#' You can tune the threshold for tuning how low counts for a given gene are
#' before they are deemed "too low" and filtered out of downstream analysis. By
#' default, this parameter is set to 1, meaning any raw count value less than 1
#' will count as "too low".
#'
#' The QC plots are provided to help you assess: (1) PCA Plot: the within and
#' between group variance in expression after dimensionality reduction; (2)
#' Count Density Histogram: the dis/similarity of count distributions between
#' samples; and (3) Similarity Heatmap: the overall similarity of samples to one
#' another based on unsupervised clustering.
#'
#'
#' @param renee_ds reneeDataSet object (see `create_reneeDataSet_from_dataframes()`)
#' @param count_type the type of counts to use -- must be a name in the counts slot (`renee_ds@counts`)
#' @param gene_names_column The column from your input Counts Matrix containing the Feature IDs (Usually Gene or Protein ID). This is usually the first column of your input Counts Matrix. Only columns of Text type from your input Counts Matrix will be available to select for this parameter.
#' @param sample_names_column The column from your input Sample Metadata table containing the sample names. The names in this column must exactly match the names used as the sample column names of your input Counts Matrix. Only columns of Text type from your input Sample Metadata table will be available to select for this parameter.
#' @param group_column The column from your input Sample Metadata table containing the sample group information. This is usually a column showing to which experimental treatments each sample belongs (e.g. WildType, Knockout, Tumor, Normal, Before, After, etc.). Only columns of Text type from your input Sample Metadata will be available to select for this parameter.
#' @param label_column The column from your input Sample Metadata table containing the sample labels as you wish them to appear in the plots produced by this template. This can be the same Sample Names Column. However, you may desire different labels to display on your figure (e.g. shorter labels are sometimes preferred on plots). In that case, select the column with your preferred Labels here. The selected column should contain unique names for each sample.
#' @param columns_to_include Which Columns would you like to include? Usually, you will choose to a feature ID column (e.g. gene or protein ID) and all sample columns. Columns excluded here will be removed in this step and from further analysis downstream of this step.
#' @param outlier_samples_to_remove A list of sample names to remove from the analysis.
#' @param use_cpm_counts_to_filter If no transformation has been been performed on counts matrix (eg Raw Counts) set to TRUE. If TRUE counts will be transformed to CPM and filtered based on given criteria. If gene counts matrix has been transformed (eg log2, CPM, FPKM or some form of Normalization) set to FALSE. If FALSE no further transformation will be applied and features will be filtered as is. For RNAseq data RAW counts should be transformed to CPM in order to properly filter.
#' @param minimum_count_value_to_be_considered_nonzero Minimum count value to be considered non-zero for a sample
#' @param minimum_number_of_samples_with_nonzero_counts_in_total Minimum number of samples (total) with non-zero counts
#' @param use_group_based_filtering If TRUE, only keeps features (e.g. genes) that have at least a certain number of samples with nonzero CPM counts in at least one group
#' @param minimum_number_of_samples_with_nonzero_counts_in_a_group Only keeps genes that have at least this number of samples with nonzero CPM counts in at least one group
#' @param make_plots whether to create plots
#' @param principal_component_on_x_axis The principle component to plot on the x-axis for the PCA plot. Choices include 1, 2, 3, ... (default: 1)
#' @param principal_component_on_y_axis The principle component to plot on the y-axis for the PCA plot. Choices include 1, 2, 3, ... (default: 2)
#' @param legend_position_for_pca legend position for the PCA plot
#' @param point_size_for_pca geom point size for the PCA plot
#' @param add_label_to_pca label points on the PCA plot
#' @param label_font_size label font size for the PCA plot
#' @param label_offset_y_ label offset y for the PCA plot
#' @param label_offset_x_ label offset x for the PCA plot
#' @param samples_to_rename_manually If you do not have a Plot Labels Column in your sample metadata table, you can use this parameter to rename samples manually for display on the PCA plot. Use "Add item" to add each additional sample for renaming. Use the following format to describe which old name (in your sample metadata table) you want to rename to which new name: old_name: new_name
#' @param color_histogram_by_group Set to FALSE to label histogram by Sample Names, or set to TRUE to label histogram by the column you select in the "Group Column Used to Color Histogram" parameter (below). Default is FALSE.
#' @param set_min_max_for_x_axis_for_histogram whether to set min/max value for histogram x-axis
#' @param minimum_for_x_axis_for_histogram x-axis minimum for histogram plot
#' @param maximum_for_x_axis_for_histogram x-axis maximum for histogram plot
#' @param legend_position_for_histogram legend position for the histogram plot. consider setting to 'none' for a large number of samples.
#' @param legend_font_size_for_histogram legend font size for the histogram plot
#' @param number_of_histogram_legend_columns number of columns for the histogram legend
#' @param colors_for_plots Colors for the PCA and histogram will be picked, in order, from this list. If you have >12 samples or groups, program will choose from a wide range of random colors
#' @param number_of_image_rows number of rows for the plot image. 1 = side-by-side, 2 = stacked
#' @param interactive_plots set to TRUE to make PCA and Histogram plots interactive with `plotly`, allowing you to hover your mouse over a point or line to view sample information. The similarity heat map will not display if this toggle is set to TRUE. Default is FALSE.
#' @param plot_correlation_matrix_heatmap Data sets with a large number of samples may be too large to create a correlation matrix heat map. If this template takes longer than 5 minutes to run, Toggle switch to FALSE and the correlation matrix will not be be created. Default is TRUE.
#'
#' @return `reneeDataSet` with filtered counts
#' @export
#'
#' @examples
#' renee_ds <- create_reneeDataSet_from_dataframes(
#'   as.data.frame(nidap_sample_metadata),
#'   as.data.frame(nidap_clean_raw_counts),
#'   sample_id_colname = "Sample"
#' ) %>%
#'   calc_cpm(gene_colname = "Gene") %>%
#'   filter_counts(
#'     sample_names_column = "Sample",
#'     gene_names_column = "Gene"
#'   )
#' head(renee_ds@counts$filt)
#'
filter_counts <- function(renee_ds,
                          count_type = "raw",
                          gene_names_column = "gene_id",
                          sample_names_column = "sample_id",
                          group_column = "Group",
                          label_column = "Label",
                          columns_to_include = c("Gene", "A1", "A2", "A3", "B1", "B2", "B3", "C1", "C2", "C3"),
                          outlier_samples_to_remove = c(),
                          minimum_count_value_to_be_considered_nonzero = 8,
                          minimum_number_of_samples_with_nonzero_counts_in_total = 7,
                          minimum_number_of_samples_with_nonzero_counts_in_a_group = 3,
                          use_cpm_counts_to_filter = TRUE,
                          use_group_based_filtering = FALSE,
                          principal_component_on_x_axis = 1,
                          principal_component_on_y_axis = 2,
                          legend_position_for_pca = "top",
                          point_size_for_pca = 1,
                          add_label_to_pca = TRUE,
                          label_font_size = 3,
                          label_offset_y_ = 2,
                          label_offset_x_ = 2,
                          samples_to_rename_manually = c(""),
                          color_histogram_by_group = FALSE,
                          set_min_max_for_x_axis_for_histogram = FALSE,
                          minimum_for_x_axis_for_histogram = -1,
                          maximum_for_x_axis_for_histogram = 1,
                          legend_position_for_histogram = "top",
                          legend_font_size_for_histogram = 10,
                          number_of_histogram_legend_columns = 6,
                          colors_for_plots = c(
                            "indigo",
                            "carrot",
                            "lipstick",
                            "turquoise",
                            "lavender",
                            "jade",
                            "coral",
                            "azure",
                            "green",
                            "rum",
                            "orange",
                            "olive"
                          ),
                          number_of_image_rows = 2,
                          interactive_plots = FALSE,
                          plot_correlation_matrix_heatmap = TRUE,
                          make_plots = TRUE) {
  counts_matrix <- renee_ds@counts[[count_type]] %>% as.data.frame() # currently, this function requires data frames
  sample_metadata <- renee_ds@sample_meta %>% as.data.frame()
  # TODO we should use "feature" instead of "gene" to make sure this is applicable beyond RNA-seq

  # TODO: just have users specify hex values directly for simplicity
  colorlist <- c(
    indigo = "#5954d6",
    carrot = "#e1562c",
    lipstick = "#b80058",
    turquoise = "#00c6f8",
    lavender = "#d163e6",
    jade = "#00a76c",
    coral = "#ff9287",
    azure = "#008cf9",
    green = "#006e00",
    rum = "#796880",
    orange = "#FFA500",
    olive = "#878500"
  )
  if (length(colors_for_plots) == 0) {
    colors_for_plots <- c(
      "indigo",
      "carrot",
      "lipstick",
      "turquoise",
      "lavender",
      "jade",
      "coral",
      "azure",
      "green",
      "rum",
      "orange",
      "olive"
    )
  }

  # purpose of this code block for samples_to_include:
  # ensure samples in metadata match columns in counts table and
  # also exclude annotation / gene columns
  # TODO separate slots in S7 for samples, counts, annotations --> create function to validate
  samples_to_include <- columns_to_include[columns_to_include %in% sample_metadata[, sample_names_column, drop = T]]
  anno_col <- columns_to_include[columns_to_include %in% sample_metadata[, sample_names_column, drop = T] == F]


  samples_to_include <- samples_to_include[!samples_to_include %in% outlier_samples_to_remove]
  samples_to_include <- samples_to_include[samples_to_include != gene_names_column]
  samples_to_include <- samples_to_include[samples_to_include != "Gene"]
  samples_to_include <- samples_to_include[samples_to_include != "GeneName"]
  samples_to_include <- samples_to_include[samples_to_include %in% sample_metadata[[sample_names_column]]]

  ## create unique rownames to correctly add back Annocolumns at end of template
  counts_matrix[, gene_names_column] <- paste0(counts_matrix[, gene_names_column], "_", 1:nrow(counts_matrix))

  anno_col <- c(anno_col, gene_names_column) %>% unique()
  anno_tbl <- counts_matrix[, anno_col, drop = F] %>% as.data.frame()

  df <- counts_matrix[, c(gene_names_column, samples_to_include)]
  gene_names <- NULL
  gene_names$GeneID <- counts_matrix[, gene_names_column]

  ### Input data validation
  # TODO move this function call to the S7 validator
  sample_metadata <- validate_sample_metadata(
    counts_matrix = df,
    sample_metadata = sample_metadata,
    sample_names_column = sample_names_column,
    group_column = group_column
  )

  #### remove low count genes ########
  df.filt <- remove_low_count_genes(
    counts_matrix = df,
    sample_metadata = sample_metadata,
    gene_names_column = gene_names_column,
    group_column = group_column,
    use_cpm_counts_to_filter = use_cpm_counts_to_filter,
    use_group_based_filtering = use_group_based_filtering,
    minimum_count_value_to_be_considered_nonzero = minimum_count_value_to_be_considered_nonzero,
    minimum_number_of_samples_with_nonzero_counts_in_total = minimum_number_of_samples_with_nonzero_counts_in_total,
    minimum_number_of_samples_with_nonzero_counts_in_a_group = minimum_number_of_samples_with_nonzero_counts_in_a_group
  )


  colorval <- colorlist[colors_for_plots]
  colorval <- unname(colorval) # remove names which affect ggplot

  if (length(unique(sample_metadata[[group_column]])) > length(colorval)) {
    ## Original color-picking code.
    k <- length(unique(sample_metadata[[group_column]])) - length(colorval)
    more_cols <- get_random_colors(k)
    colorval <- c(colorval, more_cols)
  }

  if (isTRUE(make_plots)) {
    log_counts <- log((as.matrix(df.filt[, samples_to_include] + 0.5)))
    rownames(log_counts) <- df.filt[, 1]

    pcaPlot <- plot_pca(
      log_counts,
      sample_metadata,
      samples_to_include,
      samples_to_rename_manually,
      group_column,
      label_column,
      color_values = colorval,
      principal_component_on_x_axis = principal_component_on_x_axis,
      principal_component_on_y_axis = principal_component_on_y_axis,
      legend_position_for_pca = legend_position_for_pca,
      point_size_for_pca = point_size_for_pca,
      add_label_to_pca = add_label_to_pca,
      label_font_size = label_font_size,
      label_offset_y_ = label_offset_y_,
      label_offset_x_ = label_offset_x_
    )

    ########################
    ## Start Histogram Plot:
    ########################
    histPlot <- plot_histogram(
      log_counts,
      sample_metadata,
      gene_names_column = gene_names_column,
      group_column = group_column,
      label_column = label_column,
      color_values = colorval,
      color_histogram_by_group = color_histogram_by_group,
      set_min_max_for_x_axis_for_histogram = set_min_max_for_x_axis_for_histogram,
      minimum_for_x_axis_for_histogram = minimum_for_x_axis_for_histogram,
      maximum_for_x_axis_for_histogram = maximum_for_x_axis_for_histogram,
      legend_position_for_histogram = legend_position_for_histogram,
      legend_font_size_for_histogram = legend_font_size_for_histogram,
      number_of_histogram_legend_columns = number_of_histogram_legend_columns
    )

    ########################
    ### Output Figures
    ########################
    if (plot_correlation_matrix_heatmap == TRUE) {
      if (interactive_plots == TRUE) {
        pcaPlot1 <- (pcaPlot) %>% plotly::ggplotly(tooltip = c("sample", "group"))
        histPlot2 <- (histPlot + ggplot2::theme(legend.position = "none")) %>%
          plotly::ggplotly(tooltip = c("sample"))

        grid::grid.newpage()
        # print(pcaPlot1)
        grid::grid.newpage()
        # print(histPlot2)
      } else {
        corHM <- plot_heatmap(
          counts_matrix = df.filt[, samples_to_include],
          sample_metadata = sample_metadata,
          sample_names_column = sample_names_column,
          label_column = label_column,
          anno_column = group_column,
          anno_colors = colorval
        )

        # grid.newpage()
        # print(pcaPlot)
        grid::grid.newpage()
        # print(corHM)
        grid::grid.newpage()
        # print(histPlot)
      }
    } else {
      if (interactive_plots == TRUE) {
        pcaPlot1 <- (pcaPlot) %>% plotly::ggplotly(tooltip = c("sample", "group"))
        histPlot2 <- (histPlot + ggplot2::theme(legend.position = "none")) %>%
          plotly::ggplotly(tooltip = "sample")

        grid::grid.newpage()
        # print(pcaPlot1)
        grid::grid.newpage()
        # print(histPlot2)
      } else {
        grid::grid.newpage()
        # print(pcaPlot)
        grid::grid.newpage()
        # print(histPlot)
      }
    }
  }
  df.final <- df %>%
    dplyr::filter(!!rlang::sym(gene_names_column) %in% df.filt[, gene_names_column])
  df.final <- merge(anno_tbl, df.final, by = gene_names_column, all.y = T)
  df.final[, gene_names_column] <- gsub("_[0-9]+$", "", df.final[, gene_names_column])

  renee_ds@counts[["filt"]] <- df.final

  return(renee_ds)
}

#' Remove low-count genes
#'
#' TODO this function also transforms raw counts to CPM, but that should be a separate function before this step, before filter_counts function()
#' TODO document `isexpr1` column in output
#'
#' @inheritParams filter_counts
#'
#' @return counts matrix with low-count genes removed
#' @keywords internal
#'
remove_low_count_genes <- function(counts_matrix,
                                   sample_metadata,
                                   gene_names_column,
                                   group_column,
                                   use_cpm_counts_to_filter = TRUE,
                                   use_group_based_filtering = FALSE,
                                   minimum_count_value_to_be_considered_nonzero = 8,
                                   minimum_number_of_samples_with_nonzero_counts_in_total = 7,
                                   minimum_number_of_samples_with_nonzero_counts_in_a_group = 3) {
  value <- NULL
  df <- counts_matrix

  df <- df[stats::complete.cases(df), ]

  ## USE CPM Transformation
  trans.df <- df
  if (use_cpm_counts_to_filter == TRUE) {
    trans.df[, -1] <- edgeR::cpm(as.matrix(df[, -1]))
  }

  if (use_group_based_filtering == TRUE) {
    rownames(trans.df) <- trans.df[, gene_names_column]
    trans.df[, gene_names_column] <- NULL

    counts <- trans.df >= minimum_count_value_to_be_considered_nonzero # boolean matrix

    tcounts <- as.data.frame(t(counts))
    colnum <- dim(counts)[1] # number of genes
    tcounts <- merge(sample_metadata[group_column], tcounts, by = "row.names")
    tcounts$Row.names <- NULL
    melted <- reshape2::melt(tcounts, id.vars = group_column)
    tcounts.tot <- dplyr::summarise(dplyr::group_by_at(melted, c(group_column, "variable")), sum = sum(value))
    tcounts.group <- tcounts.tot %>%
      tidyr::pivot_wider(names_from = "variable", values_from = "sum")
    colSums(tcounts.group[(1:colnum + 1)] >= minimum_number_of_samples_with_nonzero_counts_in_a_group) >= 1 -> tcounts.keep
    df.filt <- trans.df[tcounts.keep, ]
    df.filt %>% tibble::rownames_to_column(gene_names_column) -> df.filt
  } else {
    trans.df$isexpr1 <- rowSums(as.matrix(trans.df[, -1]) > minimum_count_value_to_be_considered_nonzero) >= minimum_number_of_samples_with_nonzero_counts_in_total

    df.filt <- as.data.frame(trans.df[trans.df$isexpr1, ])
  }

  # colnames(df.filt)[colnames(df.filt)==gene_names_column] <- "Gene"
  # print(paste0("Number of features after filtering: ", nrow(df.filt)))
  return(df.filt)
}
