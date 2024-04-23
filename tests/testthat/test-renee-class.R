test_that("reneeDataSet from files works", {
  rds <- create_reneeDataSet_from_files(
    system.file("extdata", "RSEM.genes.expected_count.all_samples.txt", package = "reneeTools"),
    system.file("extdata", "sample_metadata.tsv", package = "reneeTools")
  )
  expect_equal(
    rds@counts %>% head(),
    structure(c(
      0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
      0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L
    ), dim = c(6L, 4L), dimnames = list(
      c(
        "ENSG00000121410.11", "ENSG00000268895.5", "ENSG00000148584.15",
        "ENSG00000175899.14", "ENSG00000245105.3", "ENSG00000166535.20"
      ), c("KO_S3", "KO_S4", "WT_S1", "WT_S2")
    ))
  )
  expect_equal(
    rds@sample_meta,
    structure(list(condition = c(
      "knockout", "knockout", "wildtype",
      "wildtype"
    )), row.names = c("KO_S3", "KO_S4", "WT_S1", "WT_S2"), class = "data.frame")
  )
})

test_that("reneeDataSet from data frames detect problems", {
  sample_meta <- data.frame(
    sample_id = c("KO_S3", "KO_S4", "WT_S1", "WT_S2"),
    condition = factor(
      c("knockout", "knockout", "wildtype", "wildtype"),
      levels = c("wildtype", "knockout")
    )
  )
  expect_error(
    create_reneeDataSet_from_dataframes(gene_counts[, 1:4], sample_meta),
    "Not all columns"
  )
})
