#' Run DESeq2 on a reneeDataSet
#'
#' @param renee_ds reneeDataSet object
#' @param design   model formula for experimental design. Columns must exist in `meta_dat`.
#' @param ...      remaining variables are forwarded to `DESeq2::DESeq()`.
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
run_deseq2 <- function(renee_ds, design, ...) {
  dds <- DESeq2::DESeqDataSetFromMatrix(
    renee_ds@counts,
    renee_ds@sample_meta,
    design
  )
  renee_ds@analyses$deseq2_ds <- DESeq2::DESeq(dds, ...)
  return(renee_ds)
}
