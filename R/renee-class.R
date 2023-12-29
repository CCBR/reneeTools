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
  count_dat <- readr::read_tsv(gene_counts_filepath)
  sample_meta_dat <- readr::read_tsv(sample_meta_filepath)
  return(reneeDataSetFromDataFrames(count_dat, sample_meta_dat))
}

#' Construct a reneeDataSet object from data frames
#'
#' @param gene_counts_dat expected gene counts from RSEM as a data frame or tibble.
#'   Must contain a `gene_id` column and a column for each sample ID in the metadata.
#' @param sample_meta_dat sample metadata as a data frame or tibble.
#'   Must contain a `sample_ID` column.
#'
#' @return reneeDataSet object
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
#' reneeDataSetFromDataFrames(gene_counts, sample_meta)
reneeDataSetFromDataFrames <- function(gene_counts_dat, sample_meta_dat) {
  count_mat <- gene_counts_dat %>% counts_dat_to_matrix()
  sample_meta_dat <- sample_meta_dat %>% meta_tbl_to_dat()

  # sample IDs must be in the same order
  if (!all(colnames(count_mat) == rownames(sample_meta_dat))) {
    stop("Not all columns in the count matrix equal the rows in the sample metadata. Sample IDs must be in the same order.")
  }

  return(reneeDataSet(count_mat, sample_meta_dat))
}
