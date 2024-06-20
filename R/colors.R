#' Get random colors
#'
#' @param k number of clusters for kmeans clustering
#'
#' @return
#' @keywords internal
#'
#' @examples
#' getourrandomcolors(5)
getourrandomcolors <- function(k) {
  seed <- 10
  n <- 2e3
  ourColorSpace <- colorspace::RGB(runif(n), runif(n), runif(n))
  ourColorSpace <- as(ourColorSpace, "LAB")
  currentColorSpace <- ourColorSpace@coords
  # Set iter.max to 20 to avoid convergence warnings.
  set.seed(seed)
  km <- stats::kmeans(currentColorSpace, k, iter.max = 20)
  return(unname(colorspace::hex(colorspace::LAB(km$centers))))
}
