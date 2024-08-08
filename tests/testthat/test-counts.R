test_that("counts_dat_to_matrix works", {
  expect_equal(
    counts_dat_to_matrix(head(gene_counts)),
    structure(
      c(
        0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
        0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L
      ),
      dim = c(6L, 4L),
      dimnames = list(
        c(
          "ENSG00000121410.11", "ENSG00000268895.5", "ENSG00000148584.15",
          "ENSG00000175899.14", "ENSG00000245105.3", "ENSG00000166535.20"
        ),
        c("KO_S3", "KO_S4", "WT_S1", "WT_S2")
      )
    )
  )
})
