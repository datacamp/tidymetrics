#' Cross by dimensions
#'
#' This function stacks an extra copy of the table for each dimension column
#' specified as an argument, replaces the value of the column with the word
#' "Alll", and finally groups by all the columns. It acts as an extended
#' \code{group_by} that allows complete summaries across each individual
#' dimension and possible combinations. It works both in-database and in-memory.
#'
#' @param tbl A table
#' @param ... A selection of columns
#' @param add Whether to leave the existing groups as well instead of replacing
#' them (by default, yes).
#'
#' @importFrom rlang :=
#'
#' @seealso \code{\link{discard_dimensions}}
#'
#' @examples
#' # Data Frame
#' library(dplyr)
#'
#' mtcars %>%
#'   cross_by_dimensions(cyl, am) %>%
#'   summarize(avg_mpg = mean(mpg))
#'
#' flights <- nycflights13::flights %>%
#'   mutate(date = as.Date(ISOdate(year, month, day)))
#'
#' # find flight delays by carrier, origin, and Overall
#' flight_summary <- nycflights13::flights %>%
#'   cross_by_dimensions(carrier, origin) %>%
#'   summarize(nb_flights = n(),
#'             avg_arr_delay = mean(arr_delay, na.rm = TRUE))
#'
#' flight_summary
#'
#' # This works well when combined with discard_dimensions, which filters for
#' # an All level and removes the column
#'
#' # Look just by carrier
#' flight_summary %>%
#'   discard_dimensions(origin)
#'
#' flight_summary %>%
#'   discard_dimensions(carrier)
#'
#' @export
cross_by_dimensions <- function(tbl, ..., add = TRUE){
  g_vars <- dplyr::group_vars(tbl)
  tbl <- tbl %>%
    ungroup()
  columns <- ensyms(...)
  for (column in columns) {
    tbl_1 <- tbl %>%
      mutate(!!column := as.character(!! column))
    tbl <- tbl %>%
      mutate(!!column := 'All') %>%
      union_all(tbl_1)
  }

  tbl %>%
    group_by_at(vars(g_vars)) %>%
    group_by(!!!columns, add = add)
}
