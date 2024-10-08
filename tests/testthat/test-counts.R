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

test_that("calc_cpm works on RENEE data", {
  sample_meta <- data.frame(
    sample_id = c("KO_S3", "KO_S4", "WT_S1", "WT_S2"),
    condition = factor(
      c("knockout", "knockout", "wildtype", "wildtype"),
      levels = c("wildtype", "knockout")
    )
  )
  renee_ds <- create_reneeDataSet_from_dataframes(
    sample_meta,
    gene_counts %>% dplyr::select(-GeneName)
  )
  renee_ds <- renee_ds %>% calc_cpm()
  cpm_edger <- gene_counts %>%
    dplyr::select(-GeneName) %>%
    counts_dat_to_matrix() %>%
    edgeR::cpm() %>%
    as.data.frame() %>%
    tibble::rownames_to_column("gene_id")
  expect_equal(renee_ds@counts$cpm, cpm_edger)
})

test_that("calc_cpm_df works on NIDAP data", {
  df <- nidap_clean_raw_counts %>% as.data.frame()
  trans.df <- df
  trans.df[, -1] <- edgeR::cpm(as.matrix(df[, -1]))

  expect_equal(
    calc_cpm_df(df, gene_colname = "Gene"),
    trans.df
  )
})
test_that("calc_cpm_df preserves rownames", {
  df <- nidap_clean_raw_counts %>%
    as.data.frame() %>%
    tail()
  trans.df <- df
  trans.df[, -1] <- edgeR::cpm(as.matrix(df[, -1]))

  expect_equal(
    calc_cpm_df(df, gene_colname = "Gene"),
    trans.df
  )
})
