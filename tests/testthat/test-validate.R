counts_rows <- data.frame(
  Gene = c(
    "RP23-271O17.1_1",
    "Gm26206_2",
    "Xkr4_3",
    "RP23-317L18.1_4",
    "RP23-317L18.4_5",
    "RP23-317L18.3_6"
  ),
  A1 = c(0, 0, 0, 0, 0, 0),
  A2 = c(0, 0, 0, 0, 0, 0),
  A3 = c(0, 0, 0, 0, 0, 0),
  B1 = c(0, 0, 0, 0, 0, 0),
  B2 = c(0, 0, 0, 0, 0, 0),
  B3 = c(0, 0, 0, 0, 0, 0),
  C1 = c(0, 0, 0, 0, 0, 0),
  C2 = c(0, 0, 0, 0, 0, 0),
  C3 = c(0, 0, 0, 0, 0, 0)
)
sample_meta <- data.frame(
  Sample = c("A1", "A2", "A3", "B1", "B2", "B3", "C1", "C2", "C3"),
  Group = c("A", "A", "A", "B", "B", "B", "C", "C", "C"),
  Replicate = c(1, 2, 3, 1, 2, 3, 1, 2, 3),
  Batch = c(1, 2, 2, 1, 1, 2, 1, 2, 2),
  Label = c("A1", "A2", "A3", "B1", "B2", "B3", "C1", "C2", "C3")
)

test_that("validate_sample_metadata works", {
  expect_equal(
    validate_sample_metadata(
      counts_rows,
      sample_meta,
      sample_names_column = "Sample",
      group_column = "Group"
    ),
    structure(
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
  )
})
test_that("validate_sample_metadata removes special characters", {
  replace_chars <- "-.*!"
  expect_equal(
    validate_sample_metadata(
      counts_rows,
      sample_meta %>% dplyr::mutate(Group = glue::glue("{Group}{replace_chars}"))
    ),
    sample_meta %>%
      dplyr::mutate(Group = glue::glue("{Group}{glue::glue_collapse(rep('_', 4))}")) %>%
      dplyr::mutate(rowname = Sample) %>%
      tibble::column_to_rownames("rowname")
  )
})
