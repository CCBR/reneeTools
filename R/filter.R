# Filter Low Counts [CCBR] (59e7ee62-1715-4276-b370-e8395168f9d8): v774
#' Title
#'
#' @param CleanRawCounts The input Counts Matrix. Usually, this will be your Cleaned Counts matrix.
#' @param metadata_ccbr_bulk_training The Sample Metadata table containing your sample metadata. At minimum, this table must include one column each of the following: Samples, Groups, Batches, and Labels. The names in the Samples column of your input Sample Metadata must match the Sample Column Names of your input Counts Matrix exactly. You may have more than one column showing different Groups by which your samples may be organized (e.g. Genotype, Response, Time, etc.).
#'
#' @return
#' @export
#'
#' @examples
filter_counts <- function(CleanRawCounts, metadata_ccbr_bulk_training) {
  ## --------- ##
  ## Libraries ##
  ## --------- ##

  library(limma)
  library(tidyverse)
  library(edgeR)
  library(ggplot2)
  library(plotly)
  library(dplyr)
  library(RColorBrewer)
  library(colorspace)
  library(stringr)
  library(RCurl)
  library(reshape2)
  library(gridExtra)
  library(amap)
  library(lattice)
  library(gplots)
  library(gridGraphics)
  library(dendsort)
  library(ComplexHeatmap)
  library(ggrepel)

  ## -------------------------------- ##
  ## User-Defined Template Parameters ##
  ## -------------------------------- ##

  # Basic Parameters:
  counts_matrix <- CleanRawCounts
  sample_metadata <- metadata_ccbr_bulk_training
  gene_names_column <- "Gene"
  columns_to_include <- c("Gene", "A1", "A2", "A3", "B1", "B2", "B3", "C1", "C2", "C3")
  sample_names_column <- "Sample"
  groups_column <- "Group"
  labels_column <- "Label"


  # Filtering Parameters:
  outlier_samples_to_remove <- c()
  use_cpm_counts_to_filter <- TRUE
  Minimum_Count_Value_to_be_Considered_Nonzero <- 8
  Minimum_Number_of_Samples_with_Nonzero_Counts_in_Total <- 7
  Use_Group_Based_Filtering <- FALSE
  Minimum_Number_of_Samples_with_Nonzero_Counts_in_a_Group <- 3

  # PCA Parameters:
  principal_component_on_x_axis <- 1
  principal_component_on_y_axis <- 2
  legend_position_for_pca <- "top"
  point_size_for_pca <- 1
  add_labels_to_pca <- TRUE
  label_font_size <- 3
  label_offset_y_ <- 2
  label_offset_x_ <- 2
  samples_to_rename_manually <- c("")

  # Histogram Parameters:
  color_histogram_by_group <- FALSE
  set_min_max_for_x_axis_for_histogram <- FALSE
  minimum_for_x_axis_for_histogram <- -1
  maximum_for_x_axis_for_histogram <- 1
  legend_position_for_histogram <- "top"
  legend_font_size_for_histogram <- 10
  number_of_histogram_legend_columns <- 6


  # Visualization Parameters:
  colors_for_plots <- c("indigo", "carrot", "lipstick", "turquoise", "lavender", "jade", "coral", "azure", "green", "rum", "orange", "olive")
  number_of_image_rows <- 2
  interactive_plots <- FALSE

  # TCGA:
  plot_correlation_matrix_heatmap <- TRUE

  ## --------------- ##
  ## Error Messages ##
  ## -------------- ##


  ## --------- ##
  ## Functions ##
  ## --------- ##

  getourrandomcolors <- function(k) {
    seed <- 10
    n <- 2e3
    ourColorSpace <- colorspace::RGB(runif(n), runif(n), runif(n))
    ourColorSpace <- as(ourColorSpace, "LAB")
    currentColorSpace <- ourColorSpace@coords
    # Set iter.max to 20 to avoid convergence warnings.
    set.seed(seed)
    km <- kmeans(currentColorSpace, k, iter.max = 20)
    return(unname(hex(LAB(km$centers))))
  }

  colorlist <- c(
    "#5954d6", "#e1562c", "#b80058",
    "#00c6f8", "#d163e6", "#00a76c",
    "#ff9287", "#008cf9", "#006e00",
    "#796880", "#FFA500", "#878500"
  )
  names(colorlist) <- c(
    "indigo", "carrot", "lipstick",
    "turquoise", "lavender", "jade",
    "coral", "azure", "green",
    "rum", "orange", "olive"
  )
  if (length(colors_for_plots) == 0) {
    colors_for_plots <- c(
      "indigo", "carrot", "lipstick",
      "turquoise", "lavender", "jade",
      "coral", "azure", "green",
      "rum", "orange", "olive"
    )
  }


  ### Heatmap Function

  make_heatmap <- function(counts_matrix, sample_metadata, anno_col, anno_column) {
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


  ## --------------- ##
  ## Main Code Block ##
  ## --------------- ##

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


  ## & Function START
  ### Input data validation

  sample_metadata <- sample_metadata[match(colnames(df), sample_metadata[[sample_names_column]]), ] # First match sample metadata to counts matrix
  sample_metadata <- sample_metadata[rowSums(is.na(sample_metadata)) != ncol(sample_metadata), ] # Remove empty rows
  sample_metadata <- sample_metadata[, colSums(is.na(sample_metadata)) == 0] # Remove empty columns
  rownames(sample_metadata) <- sample_metadata[[sample_names_column]]


  ### Remove specal characters from Metadata Column. Replace with _
  sample_metadata[, groups_column] <- gsub("-| |!|\\*|\\.", "_", sample_metadata[, groups_column])

  ## & Function End


  ## & Function Start
  #### remove low count genes ########

  df <- df[complete.cases(df), ]
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

    counts <- trans.df > Minimum_Count_Value_to_be_Considered_Nonzero # boolean matrix

    tcounts <- as.data.frame(t(counts))
    colnum <- dim(counts)[1] # number of genes
    tcounts <- merge(sample_metadata[groups_column], tcounts, by = "row.names")
    tcounts$Row.names <- NULL
    melted <- melt(tcounts, id.vars = groups_column)
    tcounts.tot <- dplyr::summarise(dplyr::group_by_at(melted, c(groups_column, "variable")), sum = sum(value))
    tcounts.tot %>% tidyr::spread(variable, sum) -> tcounts.group
    colSums(tcounts.group[(1:colnum + 1)] >= Minimum_Number_of_Samples_with_Nonzero_Counts_in_a_Group) >= 1 -> tcounts.keep
    df.filt <- trans.df[tcounts.keep, ]
    df.filt %>% rownames_to_column(gene_names_column) -> df.filt
  } else {
    trans.df$isexpr1 <- rowSums(as.matrix(trans.df[, -1]) > Minimum_Count_Value_to_be_Considered_Nonzero) >= Minimum_Number_of_Samples_with_Nonzero_Counts_in_Total

    df.filt <- as.data.frame(trans.df[trans.df$isexpr1, ])
  }

  # colnames(df.filt)[colnames(df.filt)==gene_names_column] <- "Gene"
  # print(paste0("Number of features after filtering: ", nrow(df.filt)))
  ## & Function End

  ######## Start PCA ###############

  ## & Function Start
  edf <- log((as.matrix(df.filt[, samples_to_include] + 0.5)))
  rownames(edf) <- df.filt[, 1]
  tedf <- t(edf)
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
  ## & Function End

  ## & Function Start
  # Manual changes to sample names
  replacements <- samples_to_rename_manually

  if (!is.null(samples_to_rename_manually)) {
    if (replacements != c("")) {
      for (x in replacements) {
        old <- strsplit(x, ": ?")[[1]][1]
        new <- strsplit(x, ": ?")[[1]][2]
        pca.df$sample <- ifelse(pca.df$sample == old, new, pca.df$sample)
      }
    }
  }
  ## & Function End

  colorval <- colorlist[colors_for_plots]
  colorval <- unname(colorval) # remove names which affect ggplot

  if (length(unique(sample_metadata[[groups_column]])) > length(colorval)) {
    ## Original color-picking code.
    k <- length(unique(sample_metadata[[groups_column]])) - length(colorval)
    more_cols <- getourrandomcolors(k)
    colorval <- c(colorval, more_cols)
  }

  ## & Function Start
  #### plot PCA
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
    scale_colour_manual(values = colorval) +
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
  ## & Function End
  # par(mfrow = c(2,1))

  ########################
  ## Start Histogram Plot:
  ########################
  ## & Function Start
  df.m <- melt(edf, id.vars = c(gene_names_column))
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
  ## & Function End

  ########################
  ### Ouput Figures
  ########################


  if (plot_correlation_matrix_heatmap == TRUE) {
    if (interactive_plots == TRUE) {
      pcaPlot1 <- (pcaPlot) %>% ggplotly(tooltip = c("sample", "group"))
      histPlot2 <- (histPlot + theme(legend.position = "none")) %>% ggplotly(tooltip = c("sample"))

      grid.newpage()
      # print(pcaPlot1)
      grid.newpage()
      # print(histPlot2)
    } else {
      ## & Function Start
      require(gridExtra)
      # gh<-make_heatmap(df.filt[,samples_to_include],sample_metadata,colorval)
      corHM <- make_heatmap(
        counts_matrix = df.filt[, samples_to_include],
        sample_metadata = sample_metadata,
        anno_col = colorval,
        anno_column = groups_column
      )
      ## & Function End

      # grid.newpage()
      # print(pcaPlot)
      grid.newpage()
      # print(corHM)
      grid.newpage()
      # print(histPlot)
    }
  } else {
    if (interactive_plots == TRUE) {
      pcaPlot1 <- (pcaPlot) %>% ggplotly(tooltip = c("sample", "group"))
      histPlot2 <- (histPlot + theme(legend.position = "none")) %>% ggplotly(tooltip = "sample")

      grid.newpage()
      # print(pcaPlot1)
      grid.newpage()
      # print(histPlot2)
    } else {
      grid.newpage()
      # print(pcaPlot)
      grid.newpage()
      # print(histPlot)
    }
  }

  df %>% filter(.data[[gene_names_column]] %in% df.filt[, gene_names_column]) -> df.final
  # colnames(df.final)[colnames(df.final)==gene_names_column] <- "Gene"

  # print('')
  # print('Sample Columns')
  # print(colnames(df.final[,!colnames(df.final)%in%gene_names_column]))
  # print('Annotation Columns')
  # print(colnames(anno_tbl))

  df.final <- merge(anno_tbl, df.final, by = gene_names_column, all.y = T)
  df.final[, gene_names_column] <- gsub("_[0-9]+$", "", df.final[, gene_names_column])

  return(df.final)
}
