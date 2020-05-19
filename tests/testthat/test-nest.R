test_that("can nest all data", {
  test_df <- data.table(a = 1:3,
                        b = 4:6,
                        c = c("a", "a", "b"))

  result_df <- test_df %>%
    nest_by.()

  expect_named(result_df, c("data"))

  result_df <- test_df %>%
    nest_by.(.key = "stuff")

  expect_named(result_df, c("stuff"))
  expect_equal(nrow(result_df), 1)
})

test_that("can nest by group", {
  test_df <- data.table(a = 1:3,
                        b = 4:6,
                        c = c("a", "a", "b"))

  result_df <- test_df %>%
    nest_by.(c)

  expect_named(result_df, c("c", "data"))

  result_df <- test_df %>%
    nest_by.(c, .key = "stuff")

  expect_named(result_df, c("c", "stuff"))
  expect_equal(class(result_df$stuff), "list")
  expect_equal(nrow(result_df), 2)
})

test_that(".keep works", {
  test_df <- data.table(a = 1:3, b = 4:6, c = c("a", "a", "b"), d = c("a", "a", "b"))

  result_df <- test_df %>%
    nest_by.(c, d, .keep = TRUE) %>%
    mutate.(num_cols = map_dbl.(data, ncol))

  expect_equal(result_df$num_cols, c(4, 4))
  expect_equal(nrow(result_df), 2)
})