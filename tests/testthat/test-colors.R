test_that("get_random_colors works", {
  set.seed(10)
  expect_equal(
    get_random_colors(5),
    c("#B85CD0", "#B4E16D", "#DC967D", "#A6DCC5", "#B5AAD3")
  )
  expect_equal(
    get_random_colors(3),
    c("#B3C4C7", "#B7D579", "#C56BC8")
  )
  expect_error(get_random_colors(0), "num_colors must be at least 1")
})
