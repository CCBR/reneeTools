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
  # convert counts tibble to matrix
  counts_mat <- counts_dat_to_matrix(counts_tbl)

  # sample IDs must be in the same order
  assertthat::are_equal(colnames(counts_mat), rownames(meta_dat))

  return(DESeq2::DESeqDataSetFromMatrix(counts_mat, meta_dat, design))
}

#' Run DESeq2 on a reneeDataSet
#'
#' @param renee_ds reneeDataSet object
#' @param design   model formula for experimental design. Columns must exist in `meta_dat`.
#'
#' @return reneeDataSet object with DESeq2 slot filled
#' @export
#'
#' @examples
#' rds <- reneeDataSetFromFiles(
#'   system.file("extdata",
#'     "RSEM.genes.expected_count.all_samples.txt",
#'     package = "reneeTools"
#'   ),
#'   system.file("extdata", "sample_metadata.tsv",
#'     package = "reneeTools"
#'   )
#' )
#' run_deseq2(rds, ~condition)
run_deseq2 <- function(renee_ds, design) {
  dds <- DESeq2::DESeqDataSetFromMatrix(
    renee_ds$counts,
    renee_ds$sample_meta,
    design
  )
  renee_ds$deseq2 <- DESeq2::DESeq(dds)
  return(renee_ds)
}
