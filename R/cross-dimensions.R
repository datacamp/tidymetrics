#' Cross by dimensions
#'
#' This function stacks an extra copy of the table for each dimension column
#' specified as an argument, replaces the value of the column with the word
#' "Alll", and finally groups by all the columns. It acts as an extended
#' `group_by` that allows complete summaries across each individual
#' dimension and possible combinations. It works both in-database and in-memory.
#'
#' @param tbl A table
#' @param ... A selection of columns
#' @param add Whether to leave the existing groups as well instead of replacing
#' them (by default, yes).
#' @param max_dimensions The number of (non-All) dimensions that each row
#' can have. This reduces the size of a metrics table, by limiting the number
#' of dimensions that can be anything besides All at the same time.
#' @param collect_fun A function to collect or materialize intermediate tables.
#'  This is useful when dealing with large tables in which case the resulting
#'  SQL queries can become very complex and expensive to execute. Materializing
#'  intermediate tables as temporary tables can improve the efficiency of
#'  the query.
#'
#' @importFrom rlang :=
#'
#' @seealso [discard_dimensions()]
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
#'   summarize(
#'     nb_flights = n(),
#'     avg_arr_delay = mean(arr_delay, na.rm = TRUE)
#'   )
#'
#' flight_summary
#'
#' flight_summary <- nycflights13::flights %>%
#'   cross_by_dimensions(carrier, origin, max_dimensions = 1) %>%
#'   summarize(
#'     nb_flights = n(),
#'     avg_arr_delay = mean(arr_delay, na.rm = TRUE)
#'   )
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
#' @export
cross_by_dimensions <- function(tbl, ..., add = TRUE, max_dimensions = NULL,
                                collect_fun = NULL) {
  g_vars <- dplyr::group_vars(tbl)

  columns <- ensyms(...)

  # Set up all columns as characters (since they can be "All")
  tbl <- tbl %>%
    ungroup() %>%
    mutate_at(vars(!!!columns), as.character)

  # Separate cases if there's a max_dimensions argument
  if (!is.null(max_dimensions)) {
    tbl <- tbl %>%
      cross_by_dimensions_limited(columns,
        max_dimensions = max_dimensions,
        collect_fun = collect_fun
      )
  } else {
    # Combine with k unions, instead of the 2 ^ n that cross_by_dimensions_limited would do
    for (column in columns) {
      tbl <- tbl %>%
        mutate(!!column := "All") %>%
        union_all(tbl)
      if (!is.null(collect_fun)) {
        tbl <- collect_fun(tbl)
      }
    }
  }

  # Regroup
  tbl %>%
    group_by_at(vars(g_vars)) %>%
    group_by(!!!columns, add = add)
}

cross_by_dimensions_limited <- function(tbl, column_symbols, max_dimensions,
                                        collect_fun = NULL) {
  columns <- purrr::map_chr(column_symbols, quo_name)

  # Get all the combinations of columns with up to n items turned to "All"
  num_not_all <- seq(length(columns) - max_dimensions, length(columns))

  cols_list <- num_not_all %>%
    purrr::map(~ utils::combn(columns, .)) %>%
    purrr::map(~ lapply(1:ncol(.), function(i) .[, i])) %>%
    purrr::reduce(c)

  d <- cols_list %>%
    purrr::map(~ mutate_at(tbl, vars(.x), ~"All"))
  if (!is.null(collect_fun)) {
    d <- d %>%
      purrr::map(collect_fun)
  }
  d %>%
    purrr::reduce(union_all)
}
