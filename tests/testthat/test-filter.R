test_that("filter_counts reproduces NIDAP results", {
  renee_ds <- create_reneeDataSet_from_dataframes(
    as.data.frame(nidap_sample_metadata),
    as.data.frame(nidap_clean_raw_counts),
    sample_id_colname = Sample
  )
  renee_ds2 <- filter_counts(
    renee_ds
  )
  counts_filt <- renee_ds2@counts$filt
  expect_true(all.equal(
    counts_filt %>%
      dplyr::arrange(desc(Gene)),
    as.data.frame(nidap_filtered_counts) %>%
      dplyr::arrange(desc(Gene))
  ))
})
