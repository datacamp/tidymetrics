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
#' @param max_dimensions The number of (non-All) dimensions that each row
#' can have. This reduces the size of a metrics table, by limiting the number
#' of dimensions that can be anything besides All at the same time.
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
#' flight_summary <- nycflights13::flights %>%
#'   cross_by_dimensions(carrier, origin, max_dimensions = 1) %>%
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
cross_by_dimensions <- function(tbl, ..., add = TRUE, max_dimensions = NULL){
  if (!is.null(max_dimensions) && max_dimensions == 1){
    tbl %>%
      cross_by_dimensions_one(..., add = add)
  } else {
    g_vars <- dplyr::group_vars(tbl)
    columns <- ensyms(...)
    tbl <- tbl %>%
      ungroup() %>%
      mutate_at(vars(!!!columns), as.character) %>%
      mutate(nb_all = 0)

    for (column in columns) {
      tbl <- tbl %>%
        mutate(!!column := 'All') %>%
        mutate(nb_all = nb_all + 1) %>%
        union_all(tbl)
    }

    if (!is.null(max_dimensions)){
      nb_all_max <- rlang::dots_n(...) - max_dimensions
      tbl <- tbl %>%
        filter(nb_all >= nb_all_max)
    }

    tbl %>%
      select(-nb_all) %>%
      group_by_at(vars(g_vars)) %>%
      group_by(!!!columns, add = add)
  }
}

cross_by_dimensions_one <- function(tbl, ..., add = TRUE){
  g_vars <- dplyr::group_vars(tbl)
  columns <- rlang::ensyms(...) %>% purrr::map_chr(quo_name)
  tbl <- tbl %>%
    ungroup() %>%
    mutate_at(vars(!!!columns), as.character)
  columns %>%
    purrr::map(~ {
      tbl %>%
        mutate_at(vars(setdiff(columns, .x)), ~ "All")
    }) %>%
    purrr::reduce(union_all) %>%
    union_all(tbl %>% mutate_at(vars(columns), ~ 'All')) %>%
    group_by_at(vars(g_vars)) %>%
    group_by_at(columns, .add = add)
}
