reneeDataSet <- S7::new_class("renee",
  properties = list(
    counts = S7::new_S3_class("matrix"),
    sample_meta = S7::class_data.frame,
    analyses = S7::class_list
  ),
  constructor = function(count_matrix, sample_meta_dat) {
    S7::new_object(S7::S7_object(),
      counts = count_matrix,
      sample_meta = sample_meta_dat,
      analyses = list()
    )
  }
)

#' Construct a reneeDataSet object from tsv files.
#'
#' @param gene_counts_filepath path to tsv file of expected gene counts from RSEM.
#' @param sample_meta_filepath path to tsv file with sample IDs and metadata for differential analysis.
#'
#' @return reneeDataSet object
#' @export
#'
#' @examples
#' reneeDataSetFromFiles(
#'   system.file("extdata", "RSEM.genes.expected_count.all_samples.txt", package = "reneeTools"),
#'   system.file("extdata", "sample_metadata.tsv", package = "reneeTools")
#' )
reneeDataSetFromFiles <- function(gene_counts_filepath, sample_meta_filepath) {
  count_mat <- readr::read_tsv(gene_counts_filepath) %>%
    counts_dat_to_matrix()
  sample_meta_dat <- readr::read_tsv(sample_meta_filepath) %>%
    meta_tbl_to_dat()

  # sample IDs must be in the same order
  assertthat::are_equal(colnames(count_mat), rownames(sample_meta_dat))

  return(reneeDataSet(count_mat, sample_meta_dat))
}
