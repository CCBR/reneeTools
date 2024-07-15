#' Get random colors
#'
#' @param num_colors number of colors to select.
#' @param n number of random RGB values to generate in the color space.
#'
#' @return vector of random colors in hex format.
#' @keywords internal
#'
#' @examples
#' set.seed(10)
#' get_random_colors(5)
get_random_colors <- function(num_colors, n = 2e3) {
  if (num_colors < 1) {
    stop("num_colors must be at least 1")
  }
  n <- 2e3
  ourColorSpace <- colorspace::RGB(runif(n), runif(n), runif(n))
  ourColorSpace <- as(ourColorSpace, "LAB")
  currentColorSpace <- ourColorSpace@coords
  # Set iter.max to 20 to avoid convergence warnings.
  km <- stats::kmeans(currentColorSpace, num_colors, iter.max = 20)
  return(unname(colorspace::hex(colorspace::LAB(km$centers))))
}
