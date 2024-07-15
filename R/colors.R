#' Get random colors
#'
#' @param k number of clusters for kmeans clustering
#' @param n number of RGB values in the colorspace
#'
#' @return vector of random colors in hex format
#' @keywords internal
#'
#' @examples
#' set.seed(10)
#' get_random_colors(5)
get_random_colors <- function(k, n = 2e3) {
  n <- 2e3
  ourColorSpace <- colorspace::RGB(runif(n), runif(n), runif(n))
  ourColorSpace <- as(ourColorSpace, "LAB")
  currentColorSpace <- ourColorSpace@coords
  # Set iter.max to 20 to avoid convergence warnings.
  km <- stats::kmeans(currentColorSpace, k, iter.max = 20)
  return(unname(colorspace::hex(colorspace::LAB(km$centers))))
}
