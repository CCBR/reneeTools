#' filter_low_counts
#'
#' @param raw_counts_matrix raw_counts_matrix object
#' @param min_counts integer number of min_counts across all samples, default 0
#' @param min_cpm float minimum cpm value, default 0
#' @param min_cpm_fraction float fraction of samples that need to satisfy min_cpm filter, default 1.0
#'
#' @return filtered_raw_count_matrix
#' @export
#'
#' @examples
filter_low_counts <- function(
    raw_counts_matrix,
    min_counts = 0,
    min_cpm = 0,
    min_cpm_fraction = 1.0) {

}
