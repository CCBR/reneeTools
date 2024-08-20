#' Calculate counts-per-million (CPM) on raw counts in a reneeDataSet
#'
#' @param renee_ds reneeDataSet object
#' @param ... additional arguments to pass to edgeR::cpm()
#'
#' @return reneeDataSet with cpm-transformed counts
#' @export
#'
#' @examples
#' sample_meta <- data.frame(
#'   sample_id = c("KO_S3", "KO_S4", "WT_S1", "WT_S2"),
#'   condition = factor(
#'     c("knockout", "knockout", "wildtype", "wildtype"),
#'     levels = c("wildtype", "knockout")
#'   )
#' )
#' renee_ds <- create_reneeDataSet_from_dataframes(sample_meta, gene_counts) %>%
#'   calc_cpm()
#' head(renee_ds@counts$cpm)
calc_cpm <- S7::new_generic("calc_cpm", "renee_ds", function(renee_ds, ...) {
  S7::S7_dispatch()
})

S7::method(calc_cpm, reneeDataSet) <- function(renee_ds, gene_colname = "gene_id", ...) {
  renee_ds@counts$cpm <- renee_ds@counts$raw %>%
    calc_cpm_df(gene_colname = gene_colname)
  return(renee_ds)
}

#' Calculate CPM on a data frame
#'
#' @param dat data frame of counts with a gene column
#' @param gene_colname name of the gene column (default: "gene_id")
#' @param ... additional arguments to pass to edger::cpm()
#'
#' @return cpm-transformed counts as a data frame
#' @keywords internal
#'
calc_cpm_df <- function(dat, gene_colname = "gene_id", ...) {
  gene_ids <- dat %>% dplyr::pull(gene_colname)
  row_names <- rownames(dat)
  dat_cpm <- dat %>%
    dplyr::select(-any_of(gene_colname)) %>%
    as.matrix() %>%
    edgeR::cpm(...) %>%
    as.data.frame()
  dat_cpm[[gene_colname]] <- gene_ids
  rownames(dat_cpm) <- if (suppressWarnings(all(!is.na(as.integer(row_names))))) {
    as.integer(row_names)
  } else {
    col
  }
  return(dat_cpm %>% relocate(all_of(gene_colname)))
}

#' Convert a data frame of gene counts to a matrix
#'
#' @param counts_tbl expected gene counts from RSEM as a data frame or tibble.
#'
#' @return matrix of gene counts with rows as gene IDs
#' @keywords internal
#'
#' @examples
#' counts_dat_to_matrix(head(gene_counts))
counts_dat_to_matrix <- function(counts_tbl, gene_colname = "gene_id") {
  gene_colnames <- c("gene_id", "GeneName", "gene_name", "Gene", gene_colname) %>%
    unique()
  counts_dat <- counts_tbl %>%
    as.data.frame()
  row.names(counts_dat) <- counts_dat %>%
    dplyr::pull(tidyselect::all_of(gene_colname))
  # convert counts tibble to matrix
  counts_mat <- counts_dat %>%
    dplyr::select(-tidyselect::any_of(gene_colnames)) %>%
    as.matrix()
  return(counts_mat)
}

#' Convert all numeric columns in a dataframe to integers
#'
#' Round doubles to integers and convert to integer type
#'
#' @param counts_tbl data frame with numeric columns
#'
#' @return data frame with any numeric columns as integers
#' @keywords internal
#'
#' @examples
#' data.frame(a = c(0, 0.1, 2.3, 5L, 6.9)) %>% as_integer_df()
as_integer_df <- function(counts_tbl) {
  counts_tbl %>%
    # deseq2 requires integer counts
    dplyr::mutate(dplyr::across(
      dplyr::where(is.numeric),
      \(x) as.integer(round(x, 0))
    ))
}
