#' Convert a data frame of gene counts to a matrix
#'
#' @param counts_tbl expected gene counts from RSEM as a data frame or tibble.
#'
#' @return matrix of gene counts with rows as gene IDs
#' @export
#'
#' @examples
#' counts_dat_to_matrix(head(gene_counts))
counts_dat_to_matrix <- function(counts_tbl, gene_colname = "gene_id") {
  gene_colnames <- c("gene_id", "GeneName", "gene_name")
  counts_dat <- counts_tbl %>%
    # deseq2 requires integer counts
    dplyr::mutate(dplyr::across(
      dplyr::where(is.numeric),
      \(x) as.integer(round(x, 0))
    )) %>%
    as.data.frame()
  row.names(counts_dat) <- counts_dat %>%
    dplyr::pull(tidyselect::all_of(gene_colname))
  # convert counts tibble to matrix
  counts_mat <- counts_dat %>%
    dplyr::select(-tidyselect::any_of(gene_colnames)) %>%
    as.matrix()
  return(counts_mat)
}
