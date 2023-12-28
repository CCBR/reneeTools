set.seed(20231228)
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
  expect_equal(
    dds@assays@data@listData %>% as.data.frame() %>% dplyr::filter(counts.KO_S3 > 15),
    structure(
      list(
        counts.KO_S3 = c(25L, 16L, 19L),
        counts.KO_S4 = c(22L, 10L, 26L),
        counts.WT_S1 = c(74L, 0L, 10L),
        counts.WT_S2 = c(104L, 0L, 8L)
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
