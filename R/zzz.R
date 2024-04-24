# source: https://rconsortium.github.io/S7/articles/packages.html#method-registration
.onLoad <- function(...) {
  S7::methods_register()
}

# enable usage of <S7_object>@name in package code
# source: https://rconsortium.github.io/S7/articles/packages.html#backward-compatibility
#' @rawNamespace if (getRversion() < "4.3.0") importFrom("S7", "@")
NULL


# Suppress R CMD check note 'All declared Imports should be used'.
# These packages are used within S7 methods.
#' @importFrom DESeq2 DESeq
NULL
