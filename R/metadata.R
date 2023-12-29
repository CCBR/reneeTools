#' Convert sample metadata from a tibble to a dataframe with sample IDs as row names
#'
#' @param meta_tbl tibble with `sample_id` column
#'
#' @return dataframe where row names are the sample IDs
#' @export
#'
#' @examples
#' sample_meta_tbl <- readr::read_tsv(system.file("extdata",
#'   "sample_metadata.tsv",
#'   package = "reneeTools"
#' ))
#' head(sample_meta_tbl)
#' meta_tbl_to_dat(sample_meta_tbl)
meta_tbl_to_dat <- function(meta_tbl) {
  sample_id <- NULL
  meta_dat <- meta_tbl %>%
    as.data.frame() %>%
    dplyr::select(-sample_id)
  rownames(meta_dat) <- meta_tbl %>% dplyr::pull(sample_id)
  return(meta_dat)
}
