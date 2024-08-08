#' Filter low counts
#'
#' @param counts_matrix The input Counts Matrix. Usually, this will be your Cleaned Counts matrix.
#' @param sample_metadata The Sample Metadata table containing your sample metadata. At minimum, this table must include one column each of the following: Samples, Groups, Batches, and Labels. The names in the Samples column of your input Sample Metadata must match the Sample Column Names of your input Counts Matrix exactly. You may have more than one column showing different Groups by which your samples may be organized (e.g. Genotype, Response, Time, etc.).
#'
#' @return Filtered counts as a dataframe
#' @export
#'
#' @examples
#' renee_ds <- create_reneeDataSet_from_dataframes(
#'   as.data.frame(nidap_sample_metadata),
#'   as.data.frame(nidap_clean_raw_counts),
#'   sample_id_colname = "Sample"
#' )
#' set.seed(10)
#' renee_ds2 <- filter_counts(renee_ds)
#' head(renee_ds2@counts[["filt"]])
#'
filter_counts <- function(renee_ds,
                          gene_names_column = "Gene",
                          sample_names_column = "Sample",
                          groups_column = "Group",
                          labels_column = "Label",
                          columns_to_include = c("Gene", "A1", "A2", "A3", "B1", "B2", "B3", "C1", "C2", "C3"),
                          outlier_samples_to_remove = c(),
                          use_cpm_counts_to_filter = TRUE,
                          Minimum_Count_Value_to_be_Considered_Nonzero = 8,
                          Minimum_Number_of_Samples_with_Nonzero_Counts_in_Total = 7,
                          Use_Group_Based_Filtering = FALSE,
                          Minimum_Number_of_Samples_with_Nonzero_Counts_in_a_Group = 3,
                          principal_component_on_x_axis = 1,
                          principal_component_on_y_axis = 2,
                          legend_position_for_pca = "top",
                          point_size_for_pca = 1,
                          add_labels_to_pca = TRUE,
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
                          colors_for_plots = c("indigo", "carrot", "lipstick", "turquoise", "lavender", "jade", "coral", "azure", "green", "rum", "orange", "olive"),
                          number_of_image_rows = 2,
                          interactive_plots = FALSE,
                          plot_correlation_matrix_heatmap = TRUE) {
  counts_matrix <- renee_ds@counts[["raw"]]
  sample_metadata <- renee_ds@sample_meta
  # TODO we should use "feature" instead of "gene" to make sure this is applicable beyond RNA-seq

  # TODO: just have users specify hex values directly for simplicity
  colorlist <- c(
    indigo = "#5954d6", carrot = "#e1562c", lipstick = "#b80058",
    turquoise = "#00c6f8", lavender = "#d163e6", jade = "#00a76c",
    coral = "#ff9287", azure = "#008cf9", green = "#006e00", rum = "#796880",
    orange = "#FFA500", olive = "#878500"
  )
  if (length(colors_for_plots) == 0) {
    colors_for_plots <- c(
      "indigo", "carrot", "lipstick",
      "turquoise", "lavender", "jade",
      "coral", "azure", "green",
      "rum", "orange", "olive"
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
    groups_column = groups_column
  )

  #### remove low count genes ########
  df.filt <- remove_low_count_genes(
    counts_matrix = df,
    sample_metadata = sample_metadata,
    gene_names_column = gene_names_column,
    groups_column = groups_column,
    use_cpm_counts_to_filter = use_cpm_counts_to_filter,
    Use_Group_Based_Filtering = Use_Group_Based_Filtering,
    Minimum_Count_Value_to_be_Considered_Nonzero = Minimum_Count_Value_to_be_Considered_Nonzero,
    Minimum_Number_of_Samples_with_Nonzero_Counts_in_Total = Minimum_Number_of_Samples_with_Nonzero_Counts_in_Total,
    Minimum_Number_of_Samples_with_Nonzero_Counts_in_a_Group = Minimum_Number_of_Samples_with_Nonzero_Counts_in_a_Group
  )


  colorval <- colorlist[colors_for_plots]
  colorval <- unname(colorval) # remove names which affect ggplot

  if (length(unique(sample_metadata[[groups_column]])) > length(colorval)) {
    ## Original color-picking code.
    k <- length(unique(sample_metadata[[groups_column]])) - length(colorval)
    more_cols <- get_random_colors(k)
    colorval <- c(colorval, more_cols)
  }


  ######## Start PCA ###############

  # TODO should this be a function?
  log_counts <- log((as.matrix(df.filt[, samples_to_include] + 0.5)))
  rownames(log_counts) <- df.filt[, 1]

  pcaPlot <- plot_pca(log_counts,
    sample_metadata,
    samples_to_include,
    samples_to_rename_manually,
    groups_column,
    labels_column,
    color_values = colorval,
    principal_component_on_x_axis = principal_component_on_x_axis,
    principal_component_on_y_axis = principal_component_on_y_axis,
    legend_position_for_pca = legend_position_for_pca,
    point_size_for_pca = point_size_for_pca,
    add_labels_to_pca = add_labels_to_pca,
    label_font_size = label_font_size,
    label_offset_y_ = label_offset_y_,
    label_offset_x_ = label_offset_x_
  )

  ########################
  ## Start Histogram Plot:
  ########################
  histPlot <- plot_histogram(log_counts, sample_metadata,
    gene_names_column = gene_names_column,
    groups_column = groups_column,
    labels_column = labels_column,
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
        labels_column = labels_column,
        anno_column = groups_column,
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

  df.final <- df %>%
    dplyr::filter(!!rlang::sym(gene_names_column) %in% df.filt[, gene_names_column])
  # colnames(df.final)[colnames(df.final)==gene_names_column] <- "Gene"

  # print('')
  # print('Sample Columns')
  # print(colnames(df.final[,!colnames(df.final)%in%gene_names_column]))
  # print('Annotation Columns')
  # print(colnames(anno_tbl))

  df.final <- merge(anno_tbl, df.final, by = gene_names_column, all.y = T)
  df.final[, gene_names_column] <- gsub("_[0-9]+$", "", df.final[, gene_names_column])

  renee_ds@counts[["filt"]] <- df.final

  return(renee_ds)
}

#' Remove low-count genes
#'
#' TODO this function also transforms raw counts to CPM, but that should be a separate function before this step
#' TODO document `isexpr1` column in output
#'
#' @inheritParams filter_counts
#'
#' @return counts matrix with low-count genes removed
#' @keywords internal
#'
remove_low_count_genes <- function(counts_matrix, sample_metadata,
                                   gene_names_column,
                                   groups_column,
                                   use_cpm_counts_to_filter = TRUE,
                                   Use_Group_Based_Filtering = FALSE,
                                   Minimum_Count_Value_to_be_Considered_Nonzero = 8,
                                   Minimum_Number_of_Samples_with_Nonzero_Counts_in_Total = 7,
                                   Minimum_Number_of_Samples_with_Nonzero_Counts_in_a_Group = 3) {
  value <- NULL
  df <- counts_matrix

  df <- df[stats::complete.cases(df), ]
  ## duplicate Rows should be removed in Clean_Raw_Counts template
  # df %>% dplyr::group_by(.data[[gene_names_column]]) %>% summarise_all(sum) %>% as.data.frame() -> df
  # print(paste0("Number of features before filtering: ", nrow(df)))

  ## USE CPM Transformation
  if (use_cpm_counts_to_filter == TRUE) {
    trans.df <- df
    trans.df[, -1] <- edgeR::cpm(as.matrix(df[, -1]))
    counts_label <- "Filtered Counts (CPM)"
  } else {
    trans.df <- df
    counts_label <- "Filtered Counts"
  }


  if (Use_Group_Based_Filtering == TRUE) {
    rownames(trans.df) <- trans.df[, gene_names_column]
    trans.df[, gene_names_column] <- NULL

    counts <- trans.df >= Minimum_Count_Value_to_be_Considered_Nonzero # boolean matrix

    tcounts <- as.data.frame(t(counts))
    colnum <- dim(counts)[1] # number of genes
    tcounts <- merge(sample_metadata[groups_column], tcounts, by = "row.names")
    tcounts$Row.names <- NULL
    melted <- reshape2::melt(tcounts, id.vars = groups_column)
    tcounts.tot <- dplyr::summarise(dplyr::group_by_at(melted, c(groups_column, "variable")), sum = sum(value))
    tcounts.group <- tcounts.tot %>%
      tidyr::pivot_wider(names_from = "variable", values_from = "sum")
    colSums(tcounts.group[(1:colnum + 1)] >= Minimum_Number_of_Samples_with_Nonzero_Counts_in_a_Group) >= 1 -> tcounts.keep
    df.filt <- trans.df[tcounts.keep, ]
    df.filt %>% tibble::rownames_to_column(gene_names_column) -> df.filt
  } else {
    trans.df$isexpr1 <- rowSums(as.matrix(trans.df[, -1]) > Minimum_Count_Value_to_be_Considered_Nonzero) >= Minimum_Number_of_Samples_with_Nonzero_Counts_in_Total

    df.filt <- as.data.frame(trans.df[trans.df$isexpr1, ])
  }

  # colnames(df.filt)[colnames(df.filt)==gene_names_column] <- "Gene"
  # print(paste0("Number of features after filtering: ", nrow(df.filt)))
  return(df.filt)
}
