#' Rename samples
#'
#' TODO this should probably be performed earlier on in the template?
#' why wait til after PCA is calculated?
#'
#' @param pca.df
#' @param samples_to_rename_manually TODO ask Phil for expected format
#'
#' @return PCA data frame with samples renamed
#' @keywords internal
#'
rename_samples <- function(pca.df, samples_to_rename_manually) {
  replacements <- samples_to_rename_manually

  if (!is.null(replacements) && length(replacements) > 0 && nchar(replacements) > 0) {
    # TODO: refactor with dplyr::rename for simplicity
    for (x in replacements) {
      old <- strsplit(x, ": ?")[[1]][1]
      new <- strsplit(x, ": ?")[[1]][2]
      pca.df$sample <- ifelse(pca.df$sample == old, new, pca.df$sample)
    }
  }
  return(pca.df)
}
