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
  renee_ds <-
    run_deseq2(renee_ds, design = ~condition, fitType = "local")
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
          0.739974960000608,
          0.717118872451827,
          1.34164078649987,
          1.69303431346171
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
        mu.1 = c(23.8682703018296, 13.1709993621292, 22.8847421174049),
        mu.2 = c(23.131035523431, 12.7641781441173, 22.1778862132993),
        mu.3 = c(78.660598086526, 0.163744933328453, 8.02129153499216),
        mu.4 = c(99.2628526338571, 0.20663190443383, 10.1221742389411),
        H.1 = c(0.507689382133648, 0.502052019764842, 0.503779946658291),
        H.2 = c(0.492310572696009, 0.497947673454118, 0.496219957488281),
        H.3 = c(0.446124908477968, 0.499997692127505, 0.459429993613616),
        H.4 = c(0.553875066333655, 0.499997692127505, 0.540569676700414),
        cooks.1 = c(0.0157542703049353, 0.0289351707384892, 0.0400561954057841),
        cooks.2 = c(0.0151418002975461, 0.0286548510838955, 0.0393806789741918),
        cooks.3 = c(0.0212216674791077, 0.130797161405866, 0.058560061396526),
        cooks.4 = c(0.026938300726748, 0.156792088506772, 0.0711568961752663)
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
