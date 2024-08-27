test_that("reneeDataSet from files works", {
  rds <- create_reneeDataSet_from_files(
    system.file("extdata", "sample_metadata.tsv", package = "reneeTools"),
    system.file("extdata", "RSEM.genes.expected_count.all_samples.txt", package = "reneeTools")
  ) %>%
    suppressMessages()
  expect_equal(
    rds@counts$raw %>% head(),
    tibble::tibble(
      gene_id = structure(
        c(
          "ENSG00000121410.11|A1BG",
          "ENSG00000268895.5|A1BG-AS1",
          "ENSG00000148584.15|A1CF",
          "ENSG00000175899.14|A2M",
          "ENSG00000245105.3|A2M-AS1",
          "ENSG00000166535.20|A2ML1"
        ),
        class = c("glue", "character")
      ),
      KO_S3 = c(0, 0, 0, 0, 0, 0),
      KO_S4 = c(0, 0, 0, 0, 0, 0),
      WT_S1 = c(0, 0, 0, 0, 0, 0),
      WT_S2 = c(0, 0, 0, 0, 0, 0)
    )
  )
  expect_equal(rds@sample_meta, tibble::tibble(
    sample_id = c("KO_S3", "KO_S4", "WT_S1", "WT_S2"),
    condition = c("knockout", "knockout", "wildtype", "wildtype")
  ))
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
    create_reneeDataSet_from_dataframes(sample_meta, gene_counts[, 1:4]),
    "Not all columns"
  )
})
