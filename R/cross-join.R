#' Cross join two tables together, including all combinations of rows
#'
#' Locally, this is equivalent to tidyr::crossing.
#'
#' @param x,y tbls to join
#' @param ... additional arguments to be passed on to
#'   \code{\link[dplyr]{full_join}} or \code{\link[tidyr]{crossing}}
#' @export
#' @examples
#' d1 <- dplyr::tibble(x = 1:3)
#' d2 <- dplyr::tibble(y = 1:2)
#' cross_join(d1, d2)
cross_join <- function(x, y, ...){
  UseMethod('cross_join', x)
}

#' @export
cross_join.tbl_sql <- function(x, y, ...){
  dplyr::full_join(x, y, by = character())
}

#' @export
cross_join.data.frame <- function(x, y, ...){
  tidyr::crossing(x, y, ...)
}

