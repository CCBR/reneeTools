test_that("filter_counts reproduces NIDAP results", {
  renee_ds <- create_reneeDataSet_from_dataframes(
    as.data.frame(nidap_sample_metadata),
    as.data.frame(nidap_clean_raw_counts),
    sample_id_colname = Sample
  )
  renee_ds2 <- filter_counts(renee_ds)
  counts_filt <- renee_ds2@counts$filt
  expect_true(all.equal(
    counts_filt %>%
      dplyr::arrange(desc(Gene)),
    as.data.frame(nidap_filtered_counts) %>%
      dplyr::arrange(desc(Gene))
  ))
})

test_that("remove_low_count_genes works", {
  df <- structure(
    list(
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
      C3 = c(8481, 1124, 2, 30893, 2, 50)
    ),
    row.names = 43275:43280,
    class = "data.frame"
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
      use_cpm_counts_to_filter = TRUE,
      Use_Group_Based_Filtering = FALSE,
      Minimum_Count_Value_to_be_Considered_Nonzero = 8,
      Minimum_Number_of_Samples_with_Nonzero_Counts_in_Total = 7,
      Minimum_Number_of_Samples_with_Nonzero_Counts_in_a_Group = 3
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
      row.names = c(43275L, 43276L, 43278L, 43280L),
      class = "data.frame"
    )
  )

  # test group filtering
  expect_equal(
    remove_low_count_genes(
      counts_matrix = df,
      sample_metadata = sample_meta,
      gene_names_column = "Gene",
      use_cpm_counts_to_filter = TRUE,
      Use_Group_Based_Filtering = TRUE,
      Minimum_Count_Value_to_be_Considered_Nonzero = 8,
      Minimum_Number_of_Samples_with_Nonzero_Counts_in_Total = 7,
      Minimum_Number_of_Samples_with_Nonzero_Counts_in_a_Group = 1
    ),
    structure(
      list(
        Gene = c(
          "mt-Nd5_43275",
          "mt-Nd6_43276",
          "mt-Te_43277",
          "mt-Cytb_43278",
          "mt-Tt_43279",
          "mt-Tp_43280"
        ),
        A1 = c(
          223274.204664998,
          31124.1702035042,
          0,
          745166.322051728,
          0,
          435.303079769289
        ),
        A2 = c(
          258022.219043532,
          33853.0491584418,
          23.8401754636914,
          707504.88723597,
          214.561579173223,
          381.442807419063
        ),
        A3 = c(
          223663.725998962,
          27527.4803038166,
          47.1764872387602,
          748454.970042931,
          0,
          306.647167051941
        ),
        B1 = c(
          172842.276513983,
          33126.7005133096,
          0,
          793610.277411573,
          0,
          420.74556113433
        ),
        B2 = c(
          232002.551390218,
          24499.1447044156,
          28.9930706561132,
          742715.490997652,
          202.951494592792,
          550.868342466151
        ),
        B3 = c(
          186091.435930457,
          30856.406954282,
          77.269800386349,
          782227.94591114,
          77.269800386349,
          669.671603348358
        ),
        C1 = c(
          159281.483191015,
          19809.2160935457,
          0,
          819678.436802831,
          0,
          1230.86391260866
        ),
        C2 = c(
          224485.749690211,
          28426.2701363073,
          99.1325898389095,
          746171.003717472,
          0,
          817.843866171004
        ),
        C3 = c(
          209138.883408956,
          27717.4985204182,
          49.3193923850858,
          761811.994476228,
          49.3193923850858,
          1232.98480962715
        )
      ),
      row.names = c(NA, -6L),
      class = "data.frame"
    )
  )

  # test minimum params
  expect_equal(
    remove_low_count_genes(
      counts_matrix = df,
      sample_metadata = sample_meta,
      gene_names_column = "Gene",
      use_cpm_counts_to_filter = TRUE,
      Use_Group_Based_Filtering = FALSE,
      Minimum_Count_Value_to_be_Considered_Nonzero = -1,
      Minimum_Number_of_Samples_with_Nonzero_Counts_in_Total = 7,
      Minimum_Number_of_Samples_with_Nonzero_Counts_in_a_Group = 3
    ),
    structure(
      list(
        Gene = c(
          "mt-Nd5_43275",
          "mt-Nd6_43276",
          "mt-Te_43277",
          "mt-Cytb_43278",
          "mt-Tt_43279",
          "mt-Tp_43280"
        ),
        A1 = c(
          223274.204664998,
          31124.1702035042,
          0,
          745166.322051728,
          0,
          435.303079769289
        ),
        A2 = c(
          258022.219043532,
          33853.0491584418,
          23.8401754636914,
          707504.88723597,
          214.561579173223,
          381.442807419063
        ),
        A3 = c(
          223663.725998962,
          27527.4803038166,
          47.1764872387602,
          748454.970042931,
          0,
          306.647167051941
        ),
        B1 = c(
          172842.276513983,
          33126.7005133096,
          0,
          793610.277411573,
          0,
          420.74556113433
        ),
        B2 = c(
          232002.551390218,
          24499.1447044156,
          28.9930706561132,
          742715.490997652,
          202.951494592792,
          550.868342466151
        ),
        B3 = c(
          186091.435930457,
          30856.406954282,
          77.269800386349,
          782227.94591114,
          77.269800386349,
          669.671603348358
        ),
        C1 = c(
          159281.483191015,
          19809.2160935457,
          0,
          819678.436802831,
          0,
          1230.86391260866
        ),
        C2 = c(
          224485.749690211,
          28426.2701363073,
          99.1325898389095,
          746171.003717472,
          0,
          817.843866171004
        ),
        C3 = c(
          209138.883408956,
          27717.4985204182,
          49.3193923850858,
          761811.994476228,
          49.3193923850858,
          1232.98480962715
        ),
        isexpr1 = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
      ),
      row.names = 43275:43280,
      class = "data.frame"
    )
  )
})
