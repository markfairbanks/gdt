#' Join two data.tables together
#'
#' @description Join two data.tables together
#'
#' @param x A data.frame or data.table
#' @param y A data.frame or data.table
#' @param by A character vector of variables to join by. If NULL, the default, the join will do a natural join, using all variables with common names across the two tables.
#' @param suffix Append created for duplicated column names when using `full_join.()`
#'
#' @return A data.table
#' @export
#'
#' @examples
#' df1 <- data.table(x = c("a","a","a","b","b"), y = 1:5)
#' df2 <- data.table(x = c("a","b"), z = 1:2)
#'
#' df1 %>% left_join.(df2)
#' df1 %>% inner_join.(df2)
#' df1 %>% right_join.(df2)
#' df1 %>% full_join.(df2)
#' df1 %>% anti_join.(df2)
left_join. <- function(x, y, by = NULL) {
  UseMethod("left_join.")
}

#' @export
left_join..default <- function(x, y, by = NULL) {
  if (!is.data.frame(x) | !is.data.frame(y)) stop("x & y must be a data.frame or data.table")
  if (!is.data.table(x)) x <- as_tidytable(x)
  if (!is.data.table(y)) y <- as_tidytable(y)

  by_x_y <- get_bys(x, y, by)

  by_x <- by_x_y[[1]]
  by_y <- by_x_y[[2]]

  on_vec <- by_x
  names(on_vec) <- by_y
  on_vec[on_vec == ""] <- names(on_vec)[on_vec == ""]

  join_message(on_vec, by)

  # Get y names
  y_names <- names(y)
  y_names <- y_names[!y_names %in% names(x)] # Names not in x
  y_names <- y_names[!y_names %in% by_y] # Remove old names
  all_names <- c(names(x), y_names)

  return_df <- y[x, on = on_vec, allow.cartesian = TRUE]

  setnames(return_df, names(on_vec), on_vec)
  setcolorder(return_df, all_names)

  as_tidytable(return_df)
}

#' @export
#' @rdname left_join.
inner_join. <- function(x, y, by = NULL) {
  UseMethod("inner_join.")
}

#' @export
inner_join..default <- function(x, y, by = NULL) {
  if (!is.data.frame(x) | !is.data.frame(y)) stop("x & y must be a data.frame or data.table")
  if (!is.data.table(x)) x <- as_tidytable(x)
  if (!is.data.table(y)) y <- as_tidytable(y)

  by_x_y <- get_bys(x, y, by)

  by_x <- by_x_y[[1]]
  by_y <- by_x_y[[2]]

  on_vec <- by_y
  names(on_vec) <- by_x

  join_message(on_vec, by)

  as_tidytable(x[y, on = on_vec, allow.cartesian = TRUE, nomatch = 0])
}

#' @export
#' @rdname left_join.
right_join. <- function(x, y, by = NULL) {
  UseMethod("right_join.")
}

#' @export
right_join..default <- function(x, y, by = NULL) {
  if (!is.data.frame(x) | !is.data.frame(y)) stop("x & y must be a data.frame or data.table")
  if (!is.data.table(x)) x <- as_tidytable(x)
  if (!is.data.table(y)) y <- as_tidytable(y)

  by_x_y <- get_bys(x, y, by)

  by_x <- by_x_y[[1]]
  by_y <- by_x_y[[2]]

  on_vec <- by_y
  names(on_vec) <- by_x

  join_message(on_vec, by)

  as_tidytable(x[y, on = on_vec, allow.cartesian = TRUE])
}

#' @export
#' @rdname left_join.
full_join. <- function(x, y, by = NULL, suffix = c(".x", ".y")) {
  UseMethod("full_join.")
}

#' @export
full_join..default <- function(x, y, by = NULL, suffix = c(".x", ".y")) {
    result_df <- join_mold(x, y, by = by, suffix = suffix,
                           all_x = TRUE, all_y = TRUE)

    start_names <- names(x)
    end_names <- names(result_df)
    end_names <- end_names[end_names %notin% start_names]

    col_order <- c(start_names, end_names)

    setcolorder(result_df, col_order)

    result_df
}

#' @export
#' @rdname left_join.
anti_join. <- function(x, y, by = NULL) {
  UseMethod("anti_join.")
}

#' @export
anti_join..default <- function(x, y, by = NULL) {
  if (!is.data.frame(x) | !is.data.frame(y)) stop("x & y must be a data.frame or data.table")
  if (!is.data.table(x)) x <- as_tidytable(x)
  if (!is.data.table(y)) y <- as_tidytable(y)

  by_x_y <- get_bys(x, y, by)

  by_x <- by_x_y[[1]]
  by_y <- by_x_y[[2]]

  on_vec <- by_y
  names(on_vec) <- by_x

  join_message(on_vec, by)

  as_tidytable(x[!y, on = on_vec, allow.cartesian = TRUE])
}

#' @export
#' @rdname left_join.
semi_join. <- function(x, y, by = NULL) {
  UseMethod("semi_join.")
}

#' @export
semi_join..default <- function(x, y, by = NULL) {
  if (!is.data.frame(x) | !is.data.frame(y)) stop("x & y must be a data.frame or data.table")
  if (!is.data.table(x)) x <- as_tidytable(x)
  if (!is.data.table(y)) y <- as_tidytable(y)

  by_x_y <- get_bys(x, y, by)

  by_x <- by_x_y[[1]]
  by_y <- by_x_y[[2]]

  on_vec <- by_y
  names(on_vec) <- by_x

  join_message(on_vec, by)

  result_df <- fsetdiff(x, x[!y, on = on_vec], all=TRUE)

  as_tidytable(result_df)
}

get_bys <- function(x, y, by = NULL) {
  names_x <- names(x)
  names_y <- names(y)

  if (is.null(by)) {
    by_x <- by_y <- intersect(names_x, names_y)

    if (length(by_x) == 0)
      abort("`by` must be supplied when `x` and `y` have no common variables.")
  } else {
    by_x <- names(by)
    by_y <- unname(by)
    if (is.null(by_x)) {
      by_x <- by_y
    }
  }

  if (any(by_x[by_x != ""] %notin% names_x)) stop("by.x columns not in x")
  if (any(by_y[by_y != ""] %notin% names_y)) stop("by.y columns not in y")

  list(by_x, by_y)
}

#' @export
#' @rdname dt_verb
#' @inheritParams left_join.
dt_left_join <- function(x, y, by = NULL) {
  deprecate_stop("0.5.2", "tidytable::dt_left_join()", "left_join.()")

  left_join.(x, y, by)
}

#' @export
#' @rdname dt_verb
#' @inheritParams inner_join.
dt_inner_join <- function(x, y, by = NULL) {
  deprecate_stop("0.5.2", "tidytable::dt_inner_join()", "inner_join.()")

  inner_join.(x, y, by)
}

#' @export
#' @rdname dt_verb
#' @inheritParams right_join.
dt_right_join <- function(x, y, by = NULL) {
  deprecate_stop("0.5.2", "tidytable::dt_right_join()", "right_join.()")

  right_join.(x, y, by)
}

#' @export
#' @rdname dt_verb
#' @inheritParams full_join.
dt_full_join <- function(x, y, by = NULL,suffix =  c(".x", ".y")) {
  deprecate_stop("0.5.2", "tidytable::dt_full_join()", "full_join.()")

  full_join.(x, y, by)
}

#' @export
#' @rdname dt_verb
#' @inheritParams anti_join.
dt_anti_join <- function(x, y, by = NULL) {
  deprecate_stop("0.5.2", "tidytable::dt_anti_join()", "anti_join.()")

  anti_join.(x, y, by)
}

join_mold <- function(x, y, by = NULL, suffix = c(".x", ".y"), all_x, all_y) {
  if (!is.data.frame(x) | !is.data.frame(y)) stop("x & y must be a data.frame or data.table")
  if (!is.data.table(x)) x <- as_tidytable(x)
  if (!is.data.table(y)) y <- as_tidytable(y)

  by_x_y <- get_bys(x, y, by)

  by_x <- by_x_y[[1]]
  by_y <- by_x_y[[2]]

  join_message(by_x, by)

  result_df <- as_tidytable(
    merge(x = x, y = y, by.x = by_x, by.y = by_y, suffixes = suffix,
          all.x = all_x, all.y = all_y,
          allow.cartesian = TRUE, sort = FALSE)
  )

  setkey(result_df, NULL)

  result_df
}

join_message <- function(on_vec, by) {

  if (is.null(by)) {
    by_cols_quoted <- encodeString(on_vec, quote = '"')

    by_cols_message <- paste0(by_cols_quoted, collapse = ", ")

    if (length(on_vec) > 1) {
      inform(glue("Joining, by = c({by_cols_message})"))
    } else {
      inform(glue("Joining, by = {by_cols_message}"))
    }
  }
}
