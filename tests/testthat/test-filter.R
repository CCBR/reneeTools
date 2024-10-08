equal_dfs <- function(x, y) {
  all(
    class(x) == class(y),
    names(x) == names(y),
    x == y,
    all.equal(lapply(x, class), lapply(y, class))
  )
}

test_that("filter_counts reproduces NIDAP results", {
  set.seed(10)
  renee_ds <- create_reneeDataSet_from_dataframes(
    as.data.frame(nidap_sample_metadata),
    as.data.frame(nidap_clean_raw_counts),
    sample_id_colname = "Sample"
  ) %>%
    calc_cpm(gene_colname = "Gene") %>%
    filter_counts(
      sample_names_column = "Sample",
      gene_names_column = "Gene"
    )
  rds_counts_filt <- renee_ds@counts$filt %>%
    dplyr::arrange(desc(Gene))
  nidap_counts_filt <- as.data.frame(nidap_filtered_counts) %>%
    dplyr::arrange(desc(Gene))

  expect_true(equal_dfs(rds_counts_filt, nidap_counts_filt))
})

# TODO get filter_counts() to work on tibbles too, not only dataframes

test_that("filter_counts works on RENEE dataset", {
  renee_ds <- create_reneeDataSet_from_files(
    system.file("extdata", "sample_metadata.tsv.gz", package = "reneeTools"),
    system.file(
      "extdata",
      "RSEM.genes.expected_count.all_samples.txt.gz",
      package = "reneeTools"
    )
  )
  rds2 <- renee_ds %>% filter_counts(
    gene_names_column = "gene_id",
    sample_names_column = "sample_id",
    group_column = "condition",
    label_column = "sample_id",
    columns_to_include = c("gene_id", "KO_S3", "KO_S4", "WT_S1", "WT_S2"),
    minimum_count_value_to_be_considered_nonzero = 1,
    minimum_number_of_samples_with_nonzero_counts_in_total = 1,
    minimum_number_of_samples_with_nonzero_counts_in_a_group = 1,
    make_plots = FALSE
  )
  expect_equal(
    rds2@counts$filt %>% head(),
    structure(
      list(
        gene_id = c(
          "ENSG00000072803.17|FBXW11",
          "ENSG00000083845.9|RPS5",
          "ENSG00000107371.13|EXOSC3",
          "ENSG00000111639.8|MRPL51",
          "ENSG00000111640.15|GAPDH",
          "ENSG00000111786.9|SRSF9"
        ),
        KO_S3 = c(2, 1, 1, 0, 0, 0),
        KO_S4 = c(0, 0, 1, 1, 1, 1),
        WT_S1 = c(0, 0, 0, 0, 0, 0),
        WT_S2 = c(0, 0, 0, 0, 0, 0)
      ),
      row.names = c(NA, 6L),
      class = "data.frame"
    )
  )
  expect_equal(
    rds2@counts$filt %>% tail(),
    structure(
      list(
        gene_id = c(
          "ENSG00000281903.2|LINC02246",
          "ENSG00000282393.1|AC016588.2",
          "ENSG00000283886.2|BX664615.2",
          "ENSG00000285413.1|AP001056.2",
          "ENSG00000286018.1|AF129075.3",
          "ENSG00000286104.1|AC016629.3"
        ),
        KO_S3 = c(0.85, 0, 1, 3, 2, 1),
        KO_S4 = c(0, 1, 0, 1, 0, 0),
        WT_S1 = c(0, 0, 0, 0, 0, 0),
        WT_S2 = c(0.71, 0, 0, 0, 0, 0)
      ),
      row.names = 286:291,
      class = "data.frame"
    )
  )
})

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
      use_cpm_counts_to_filter = TRUE,
      use_group_based_filtering = FALSE,
      minimum_count_value_to_be_considered_nonzero = 8,
      minimum_number_of_samples_with_nonzero_counts_in_total = 7,
      minimum_number_of_samples_with_nonzero_counts_in_a_group = 3
    ),
    structure(
      list(
        Gene = c(
          "mt-Nd5_43275",
          "mt-Nd6_43276",
          "mt-Cytb_43278",
          "mt-Tp_43280"
        ),
        A1 = c(
          223274.204664998,
          31124.1702035042,
          745166.322051728,
          435.303079769289
        ),
        A2 = c(
          258022.219043532,
          33853.0491584418,
          707504.88723597,
          381.442807419063
        ),
        A3 = c(
          223663.725998962,
          27527.4803038166,
          748454.970042931,
          306.647167051941
        ),
        B1 = c(
          172842.276513983,
          33126.7005133096,
          793610.277411573,
          420.74556113433
        ),
        B2 = c(
          232002.551390218,
          24499.1447044156,
          742715.490997652,
          550.868342466151
        ),
        B3 = c(
          186091.435930457,
          30856.406954282,
          782227.94591114,
          669.671603348358
        ),
        C1 = c(
          159281.483191015,
          19809.2160935457,
          819678.436802831,
          1230.86391260866
        ),
        C2 = c(
          224485.749690211,
          28426.2701363073,
          746171.003717472,
          817.843866171004
        ),
        C3 = c(
          209138.883408956,
          27717.4985204182,
          761811.994476228,
          1232.98480962715
        ),
        isexpr1 = c(TRUE, TRUE, TRUE, TRUE)
      ),
      row.names = c(5L, 6L, 8L, 10L),
      class = "data.frame"
    )
  )
})
