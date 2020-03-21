#' Add new variables and drop all others
#'
#' @description
#' Unlike `dt_mutate()`, `dt_transmute()` keeps only the variables that you create
#'
#' @param .data A data.frame or data.table
#' @param ... Columns to create/modify
#' @param by Optional: `list()` of bare column names to group by
#'
#' @md
#' @examples
#' # transmute keeps only the variables you create
#' mtcars %>%
#'   dt_transmute(displ_l = disp / 61.0237)
#'
#' @export
dt_transmute <- function(.data, ..., by = NULL) {
  UseMethod("dt_transmute")
}

#' @export
dt_transmute.tidytable <- function(.data, ..., by = NULL) {

  dots <- enexprs(...)
  by <- enexpr(by)
  by <- vec_selector_by(.data, !!by)

  eval_expr(
    .data[, list(!!!dots), by = !!by]
  )
}

#' @export
dt_transmute.data.frame <- function(.data, ..., by = NULL) {

  .data <- as_tidytable(.data)
  by <- enexpr(by)

  dt_transmute(.data, ..., by = !!by)
}