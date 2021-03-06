#' Number of observations in each group
#'
#' @description
#' Helper function that can be used to find counts by group.
#'
#' Can be used inside `summarize.()`, `mutate.()`, & `filter.()`
#'
#' @export
#'
#' @examples
#' test_df <- data.table(
#'   x = 1:3,
#'   y = 4:6,
#'   z = c("a","a","b")
#'  )
#'
#' test_df %>%
#'   summarize.(count = n.(), .by = z)
#'
#' # The dplyr version `n()` also works
#' test_df %>%
#'   summarize.(count = n(), .by = z)
n. <- function() {
  abort("n.() should only be used inside tidytable verbs")
}
