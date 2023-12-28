#' Create DESeq2 object from gene counts and sample metadata
#'
#' @param counts_tbl expected gene counts from RSEM as a data frame or tibble.
#' @param meta_dat   sample metadata as a data frame with rownames as sample IDs.
#' @param design     model formula for experimental design. Columns must exist in `meta_dat`.
#'
#' @return DESeqDataSet
#' @export
#'
#' @examples
#' sample_meta <- data.frame(
#'   row.names = c("KO_S3", "KO_S4", "WT_S1", "WT_S2"),
#'   condition = factor(c("knockout", "knockout", "wildtype", "wildtype"),
#'     levels = c("wildtype", "knockout")
#'   )
#' )
#' dds <- create_deseq_obj(gene_counts, sample_meta, ~condition)
create_deseq_obj <- function(counts_tbl, meta_dat, design) {
  counts_dat <- counts_tbl %>%
    # deseq2 requires integer counts
    dplyr::mutate(dplyr::across(
      dplyr::where(is.numeric),
      \(x) as.integer(round(x, 0))
    )) %>%
    as.data.frame()
  row.names(counts_dat) <- counts_dat %>% dplyr::pull("gene_id")
  # convert counts tibble to matrix
  counts_mat <- counts_dat %>%
    dplyr::select(-c(gene_id, GeneName)) %>%
    as.matrix()

  # sample IDs must be in the same order
  assertthat::are_equal(colnames(counts_mat), rownames(meta_dat))

  return(DESeq2::DESeqDataSetFromMatrix(counts_mat, meta_dat, design))
}
