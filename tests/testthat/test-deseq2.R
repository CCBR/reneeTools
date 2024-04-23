set.seed(20231228)
test_that("run_deseq2 works", {
  renee_ds <- create_reneeDataSet_from_files(
    system.file(
      "extdata",
      "RSEM.genes.expected_count.all_samples.txt",
      package = "reneeTools"
    ),
    system.file("extdata", "sample_metadata.tsv",
      package = "reneeTools"
    )
  )
  renee_ds@sample_meta <- renee_ds@sample_meta %>%
    dplyr::mutate(condition = factor(condition,
      levels = c("wildtype", "knockout")
    ))
  renee_ds <- run_deseq2(renee_ds, design = ~condition, fitType = "local")
  dds <- renee_ds@analyses$deseq2_ds
  expect_equal(
    dds@colData %>% as.data.frame(),
    structure(
      list(
        condition = structure(
          c(2L, 2L, 1L, 1L),
          levels = c(
            "wildtype",
            "knockout"
          ),
          class = "factor"
        ),
        sizeFactor = c(
          0.759835685651593,
          0.718608223926169,
          1.24466595457696,
          1.68179283050743
        )
      ),
      class = "data.frame",
      row.names = c(
        "KO_S3",
        "KO_S4", "WT_S1", "WT_S2"
      )
    )
  )
  expect_equal(
    dds@assays@data@listData %>% as.data.frame() %>% dplyr::filter(counts.KO_S3 > 15),
    structure(
      list(
        counts.KO_S3 = c(25L, 16L, 19L),
        counts.KO_S4 = c(
          22L,
          10L, 26L
        ),
        counts.WT_S1 = c(74L, 0L, 10L),
        counts.WT_S2 = c(
          104L,
          0L, 8L
        ),
        mu.1 = c(24.1516682192168, 13.3092987017581, 23.1890203839711),
        mu.2 = c(22.8412375617529, 12.5871575689046, 21.9308214491443),
        mu.3 = c(75.6181929866826, 0.158242295472865, 7.74702804399805),
        mu.4 = c(102.175314069834, 0.213817014139956, 10.4677854923446),
        H.1 = c(0.511844332569779, 0.504138999697003, 0.50668916671393),
        H.2 = c(0.48815561528062, 0.495860729372594, 0.493310737076545),
        H.3 = c(0.454896663393873, 0.499997728127273, 0.447711231007698),
        H.4 = c(0.545103297274594, 0.499997728127273, 0.552288438770429),
        cooks.1 = c(
          0.00787282712563543,
          0.0262422886768066,
          0.0478403297046226
        ),
        cooks.2 = c(0.00740399240405431, 0.025741204681852, 0.0464156166543046),
        cooks.3 = c(0.00250160501520332, 0.127533456822227, 0.0779681548830785),
        cooks.4 = c(0.00307311736197857, 0.161328557073165, 0.100411826759898)
      ),
      class = "data.frame",
      row.names = c(
        "ENSG00000185658.13",
        "ENSG00000233922.2",
        "ENSG00000157601.14"
      )
    )
  )
})
