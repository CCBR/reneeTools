#' filter_low_counts
#'
#' @param counts_dat expected gene counts from RSEM as a data frame or tibble
#' @param min_count integer number of minimum counts across all samples (default: 0)
#'
#' @return filtered counts dataframe
#' @export
#'
#' @examples
#' filter_low_counts(gene_counts) %>% head()
#' filter_low_counts(gene_counts, min_counts = 100)
filter_low_counts <- function(
    counts_dat,
    min_count = 0) {
  gene_id <- count <- count_sum <- NULL
  genes_above_threshold <- counts_dat %>%
    tidyr::pivot_longer(!c("gene_id", "GeneName"),
      names_to = "sample_id", values_to = "count"
    ) %>%
    dplyr::group_by(gene_id) %>%
    dplyr::summarize(count_sum = sum(count)) %>%
    dplyr::filter(count_sum >= min_count) %>%
    dplyr::pull(gene_id)
  return(
    counts_dat %>%
      dplyr::filter(gene_id %in% (genes_above_threshold))
  )
}
