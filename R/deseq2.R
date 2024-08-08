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
#' \dontrun{
#' renee_ds <- create_reneeDataSet_from_files(
#'   system.file("extdata", "sample_metadata.tsv",
#'     package = "reneeTools"
#'   ),
#'   system.file("extdata",
#'     "RSEM.genes.expected_count.all_samples.txt",
#'     package = "reneeTools"
#'   )
#' ) %>% filter_counts()
#' renee_ds <- run_deseq2(renee_ds, ~condition)
#' }
run_deseq2 <- S7::new_generic("run_deseq2", "renee_ds", function(renee_ds, design, ...) {
  S7::S7_dispatch()
})

S7::method(run_deseq2, reneeDataSet) <- function(renee_ds, design, min_count = 10, ...) {
  if (is.null(renee_ds@counts$filt)) {
    stop("renee_ds must contain filtered counts for DESeq2. Hint: Did you forget to run filter_counts()?")
  }
  dds <- DESeq2::DESeqDataSetFromMatrix(
    countData = renee_ds@counts$filt %>% counts_dat_to_matrix(),
    colData = renee_ds@sample_meta,
    design = design
  )
  renee_ds@analyses$deseq2_ds <- DESeq2::DESeq(dds, ...)
  renee_ds@analyses$deseq2_results <- DESeq2::results(renee_ds@analyses$deseq2_ds)
  return(renee_ds)
}
