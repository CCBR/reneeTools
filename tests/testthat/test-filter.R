test_that("filter_counts reproduces NIDAP results", {
  set.seed(10)
  renee_ds <- create_reneeDataSet_from_dataframes(
    as.data.frame(nidap_sample_metadata),
    as.data.frame(nidap_clean_raw_counts),
    sample_id_colname = "Sample"
  ) %>%
    calc_cpm(gene_colname = "Gene") %>%
    filter_counts()
  rds_counts_filt <- renee_ds@counts$filt %>%
    dplyr::arrange(desc(Gene))
  nidap_counts_filt <- as.data.frame(nidap_filtered_counts) %>%
    dplyr::arrange(desc(Gene))

  expect_true(all.equal(
    rds_counts_filt,
    nidap_counts_filt
  ))

  set.seed(10)
  renee_ds2 <- create_reneeDataSet_from_dataframes(
    as.data.frame(nidap_sample_metadata),
    as.data.frame(nidap_clean_raw_counts),
    sample_id_colname = "Sample"
  ) %>% filter_counts(renee_ds, count_type = "raw")
  counts_filt <- renee_ds2@counts$filt %>%
    as.data.frame() %>%
    dplyr::arrange(desc(Gene))
  nidap_filt <- as.data.frame(nidap_filtered_counts) %>%
    dplyr::arrange(desc(Gene))
  expect_true(all(nidap_filt == counts_filt))
  expect_true(all.equal(lapply(nidap_filt, class), lapply(counts_filt, class)))
})

# TODO get filter_counts() to work on tibbles too, not only dataframes

# TODO fails on RENEE dataset, R session aborts
# test_that('filter_counts works on RENEE dataset', {
#   renee_ds <- create_reneeDataSet_from_files(
#     system.file("extdata", "sample_metadata.tsv", package = "reneeTools"),
#     system.file("extdata", "RSEM.genes.expected_count.all_samples.txt", package = "reneeTools")
#   )
#   rds2 <- renee_ds %>% filter_counts(gene_names_column = 'gene_id',
#                              sample_names_column = 'sample_id',
#                              group_column = 'condition',
#                              label_column = 'sample_id',
#                              columns_to_include = c("gene_id", "KO_S3", "KO_S4", "WT_S1", "WT_S2"))
# })

test_that("remove_low_count_genes works", {
  df <- data.frame(
    Gene = c(
      "mt-Nd5_43275",
      "mt-Nd6_43276",
      "mt-Te_43277",
      "mt-Cytb_43278",
      "mt-Tt_43279",
      "mt-Tp_43280"
    ),
    A1 = c(6155, 858, 0, 20542, 0, 12),
    A2 = c(10823, 1420, 1, 29677, 9, 16),
    A3 = c(9482, 1167, 2, 31730, 0, 13),
    B1 = c(6162, 1181, 0, 28293, 0, 15),
    B2 = c(8002, 845, 1, 25617, 7, 19),
    B3 = c(7225, 1198, 3, 30370, 3, 26),
    C1 = c(4141, 515, 0, 21310, 0, 32),
    C2 = c(9058, 1147, 4, 30108, 0, 33),
    C3 = c(8481, 1124, 2, 30893, 2, 50),
    row.names = seq(5, 10)
  )
  df_cpm <- df %>% calc_cpm_df(gene_colname = "Gene")
  sample_meta <- structure(
    list(
      Sample = c("A1", "A2", "A3", "B1", "B2", "B3", "C1", "C2", "C3"),
      Group = c("A", "A", "A", "B", "B", "B", "C", "C", "C"),
      Replicate = c(1, 2, 3, 1, 2, 3, 1, 2, 3),
      Batch = c(1, 2, 2, 1, 1, 2, 1, 2, 2),
      Label = c("A1", "A2", "A3", "B1", "B2", "B3", "C1", "C2", "C3")
    ),
    row.names = c("A1", "A2", "A3", "B1", "B2", "B3", "C1", "C2", "C3"),
    class = "data.frame"
  )

  # test default params
  expect_equal(
    remove_low_count_genes(
      counts_matrix = df,
      sample_metadata = sample_meta,
      gene_names_column = "Gene",
      group_column = "Group",
      Use_Group_Based_Filtering = FALSE,
      Minimum_Count_Value_to_be_Considered_Nonzero = 8,
      Minimum_Number_of_Samples_with_Nonzero_Counts_in_Total = 7,
      Minimum_Number_of_Samples_with_Nonzero_Counts_in_a_Group = 3
    ),
    old_filt_cpm(
      counts_matrix = df,
      sample_metadata = sample_meta,
      gene_names_column = "Gene",
      group_column = "Group",
      use_cpm_counts_to_filter = FALSE,
      Use_Group_Based_Filtering = FALSE,
      Minimum_Count_Value_to_be_Considered_Nonzero = 8,
      Minimum_Number_of_Samples_with_Nonzero_Counts_in_Total = 7,
      Minimum_Number_of_Samples_with_Nonzero_Counts_in_a_Group = 3
    )
  )
  # test cpm
  expect_equal(
    remove_low_count_genes(
      counts_matrix = df_cpm,
      sample_metadata = sample_meta,
      gene_names_column = "Gene",
      group_column = "Group",
      Use_Group_Based_Filtering = FALSE,
      Minimum_Count_Value_to_be_Considered_Nonzero = 8,
      Minimum_Number_of_Samples_with_Nonzero_Counts_in_Total = 7,
      Minimum_Number_of_Samples_with_Nonzero_Counts_in_a_Group = 3
    ),
    old_filt_cpm(
      counts_matrix = df,
      sample_metadata = sample_meta,
      gene_names_column = "Gene",
      group_column = "Group",
      use_cpm_counts_to_filter = TRUE,
      Use_Group_Based_Filtering = FALSE,
      Minimum_Count_Value_to_be_Considered_Nonzero = 8,
      Minimum_Number_of_Samples_with_Nonzero_Counts_in_Total = 7,
      Minimum_Number_of_Samples_with_Nonzero_Counts_in_a_Group = 3
    )
  )
})
