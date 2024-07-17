#' reneeDataSet class
#'
#' @param count_dat expected gene counts from RSEM as a data frame or tibble.
#'   Must contain a `gene_id` column and a column for each sample ID in the metadata.
#' @param sample_meta_dat sample metadata as a data frame or tibble.
#'   Must contain a `sample_ID` column.
#'
#'
reneeDataSet <- S7::new_class("renee",
  properties = list(
    sample_meta = S7::class_data.frame,
    counts = S7::class_list, # list of data frames
    analyses = S7::class_list
  ),
  constructor = function(sample_meta_dat, counts_lst) {
    S7::new_object(S7::S7_object(),
      sample_meta = sample_meta_dat,
      counts = counts_lst,
      analyses = list()
    )
  },
  validator = function(self) {
    # counts must only contain approved names
    if (!all(names(self@counts) %in% c("raw", "filt", "norm_cpm"))) {
      stop("counts can only contain 'raw', 'filt', and 'norm_cpm' data frames")
    }
    # all sample IDs must be in both sample_meta and raw counts
    # any sample ID in filt or norm_cpm counts must also be in sample_meta
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
#' create_reneeDataSet_from_files(
#'   system.file("extdata", "sample_metadata.tsv", package = "reneeTools"),
#'   system.file("extdata", "RSEM.genes.expected_count.all_samples.txt", package = "reneeTools")
#' )
create_reneeDataSet_from_files <- function(sample_meta_filepath, gene_counts_filepath,
                                           count_type = "raw") {
  count_dat <- readr::read_tsv(gene_counts_filepath)
  sample_meta_dat <- readr::read_tsv(sample_meta_filepath)
  return(create_reneeDataSet_from_dataframes(sample_meta_dat, list(count_type = count_dat)))
}

#' Construct a reneeDataSet object from data frames
#'
#' @inheritParams reneeDataSet
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
#' create_reneeDataSet_from_dataframes(sample_meta, gene_counts)
create_reneeDataSet_from_dataframes <- function(sample_meta_dat,
                                                count_dat,
                                                sample_id_colname = sample_id,
                                                count_type = "raw") {
  gene_id <- GeneName <- NULL

  # sample_meta_dat <- sample_meta_dat %>% meta_tbl_to_dat(sample_id_colname = {{ sample_id_colname }})


  # sample IDs must be in the same order
  if (!all(colnames(count_dat %>% dplyr::select(-c(gene_id, GeneName, Gene))) == (sample_meta_dat %>% dplyr::pull({{ sample_id_colname }})))) {
    stop("Not all columns in the count data equal the rows in the sample metadata. Sample IDs must be in the same order.")
  }
  counts <- list()
  counts[[count_type]] <- count_dat

  return(reneeDataSet(sample_meta_dat, counts))
}
