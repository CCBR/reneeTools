% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/counts.R
\name{counts_dat_to_matrix}
\alias{counts_dat_to_matrix}
\title{Convert a data frame of gene counts to a matrix}
\usage{
counts_dat_to_matrix(counts_tbl, gene_colname = "gene_id")
}
\arguments{
\item{counts_tbl}{expected gene counts from RSEM as a data frame or tibble.}
}
\value{
matrix of gene counts with rows as gene IDs
}
\description{
Convert a data frame of gene counts to a matrix
}
\examples{
\dontrun{
counts_dat_to_matrix(head(gene_counts))
}
}
\keyword{internal}
