log_counts <- structure(
  c(
    4.68203758078952, 4.85622577980595, 4.78525385564269,
    4.49631900610197, 4.06045359796882, 4.23984554358195, 4.5945977606547,
    4.64452454872198, 5.01435137189661, 4.97202092328668, 4.63530665635318,
    4.2685504137767, 4.55963475259408, 4.48551456875509, 4.8871583679813,
    4.80572893488115, 4.62485692334614, 3.28500783834613, 4.27910086185478,
    4.44991091582054, 5.1543875974913, 5.04097648388479, 3.93341992672261,
    3.60201340344135, 4.38819757498796, 4.52837375303351, 5.04374399504142,
    5.16774070067169, 4.88762227733767, 3.4002608223214, 4.86037842060906,
    5.24497940734463, 4.91946017962122, 5.04632856830695, 4.96825190546972,
    4.23362208987531, 4.11790170891073, 4.05303076635639, 4.92868500786216,
    4.94726383191083, 3.80832003906478, 4.59490650795107, 4.85738793032812,
    4.40655427188147, 4.22382743940071, 4.58770826515446, 4.96057466313368,
    4.99824283163569, 4.5437556841171, 4.97124300205374, 4.93192370480255,
    4.13835014251084, 5.0939015418123, 5.03285949305828
  ),
  dim = c(6L, 9L),
  dimnames = list(
    c("Mrpl15_32", "Lypla1_34", "Tcea1_36", "Atp6v1h_44", "Rb1cc1_54", "Pcmtd1_68"),
    c("A1", "A2", "A3", "B1", "B2", "B3", "C1", "C2", "C3")
  )
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
test_that("plot_histogram works", {
  p <- plot_histogram(
    log_counts,
    sample_meta,
    gene_names_column = "Gene",
    group_column = "Group",
    label_column = "Label",
    color_values = c(
      indigo = "#5954d6", carrot = "#e1562c", lipstick = "#b80058",
      turquoise = "#00c6f8", lavender = "#d163e6", jade = "#00a76c",
      coral = "#ff9287", azure = "#008cf9", green = "#006e00", rum = "#796880",
      orange = "#FFA500", olive = "#878500"
    ),
    color_histogram_by_group = FALSE,
    set_min_max_for_x_axis_for_histogram = FALSE,
    minimum_for_x_axis_for_histogram = -1,
    maximum_for_x_axis_for_histogram = 1,
    legend_position_for_histogram = "top",
    legend_font_size_for_histogram = 10,
    number_of_histogram_legend_columns = 6
  )

  expect_s3_class(p$layers[[1]], "ggproto")
  expect_s3_class(p$layers[[1]]$geom, "GeomArea")
})
