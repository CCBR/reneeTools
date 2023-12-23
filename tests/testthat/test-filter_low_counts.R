test_that("filter_low_counts works", {
  test_dat <- data.frame(
    gene_id = c("A", "B", "C"),
    GeneName = c("geneA", "geneB", "geneC"),
    s1 = c(0, 0, 0),
    s2 = c(0, 1, 0),
    s3 = c(0, 0, 3)
  )
  expect_equal(filter_low_counts(test_dat), test_dat)
  expect_equal(
    filter_low_counts(test_dat, min_counts = 1),
    data.frame(
      gene_id = c("B", "C"),
      GeneName = c("geneB", "geneC"),
      s1 = c(0, 0), s2 = c(1, 0), s3 = c(0, 3)
    )
  )
  expect_equal(
    filter_low_counts(test_dat, min_counts = 2),
    data.frame(
      gene_id = "C", GeneName = "geneC",
      s1 = 0, s2 = 0, s3 = 3
    )
  )
  expect_equal(
    filter_low_counts(test_dat, min_counts = 5),
    data.frame(
      gene_id = character(0),
      GeneName = character(0),
      s1 = numeric(0), s2 = numeric(0), s3 = numeric(0)
    )
  )
})
