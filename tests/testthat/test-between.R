test_that("can filter the data.table", {
  df <- data.table(x = 1:10)

  df <- df %>%
    filter.(between.(x, 1, 4))

  expect_equal(df$x, 1:4)
})

test_that("can filter a data.frame", {
  df <- data.frame(x = 1:10)

  df <- df %>%
    filter.(between.(x, 1, 4))

  expect_equal(df$x, 1:4)
})

test_that("can filter multiple conditions with commas", {
  df <- data.table(x = 1:10, y = 1:10)

  df <- df %>%
    filter.(between.(x, 1, 4), between.(y, 1, 2))

  expect_equal(df$x, 1:2)
  expect_equal(df$y, 1:2)
})
