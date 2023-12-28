test_that("create_deseq_obj works", {
  sample_meta <-
    data.frame(
      row.names = c("KO_S3", "KO_S4", "WT_S1", "WT_S2"),
      condition = factor(
        c("knockout", "knockout", "wildtype", "wildtype"),
        levels = c("wildtype", "knockout")
      )
    )
  dds <- create_deseq_obj(gene_counts, sample_meta, ~condition)

  expect_equal(
    dds@colData %>% as.data.frame(),
    structure(
      list(condition = structure(
        c(2L, 2L, 1L, 1L),
        levels = c(
          "wildtype",
          "knockout"
        ),
        class = "factor"
      )),
      class = "data.frame",
      row.names = c(
        "KO_S3",
        "KO_S4", "WT_S1", "WT_S2"
      )
    )
  )

  # run deseq
  dds_dex <- DESeq2::DESeq(dds)
  res <- DESeq2::results(dds_dex)
  res_dat <- res %>% as.data.frame()
  expect_equal(
    res_dat %>% dplyr::filter(baseMean > 30),
    structure(
      list(
        baseMean = c(
          46.2022644566773,
          33.9841778188053,
          31.0208397331822,
          30.6175122400626,
          35.6755294108162
        ),
        log2FoldChange = c(
          -0.93461130443384, -3.35053555731274,
          -4.88594876484931,
          -4.43532926024883,
          -1.8328208411504
        ),
        lfcSE = c(
          0.267983882511375,
          0.52224772094996,
          0.860982558001587,
          0.753983075954482,
          0.36162525207182
        ),
        stat = c(
          -3.48756535533128, -6.41560589526015,
          -5.67485220164042,
          -5.88253158684504,
          -5.0682877665479
        ),
        pvalue = c(
          0.000487439776013235,
          1.40263874605495e-10,
          1.38808448089501e-08,
          4.04038197166405e-09,
          4.01410224129028e-07
        ),
        padj = c(
          0.00389951820810588,
          1.79537759495034e-08,
          5.92249378515205e-07,
          2.58584446186499e-07,
          8.56341811475261e-06
        )
      ),
      class = "data.frame",
      row.names = c(
        "ENSG00000185658.13",
        "ENSG00000142156.14",
        "ENSG00000142173.15",
        "ENSG00000159140.20",
        "ENSG00000182670.13"
      )
    )
  )
})
