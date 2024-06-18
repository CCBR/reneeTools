test_that("filter_counts reproduces NIDAP results", {
  counts_filt <- filter_counts(
    as.data.frame(nidap_clean_raw_counts),
    as.data.frame(nidap_sample_metadata)
  )
  expect_true(all.equal(
    counts_filt %>%
      dplyr::arrange(desc(Gene)),
    as.data.frame(nidap_filtered_counts) %>%
      dplyr::arrange(desc(Gene))
  ))
})
