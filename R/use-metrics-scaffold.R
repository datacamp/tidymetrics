#' Create a scaffold for documenting metrics
#'
#' Use this to generate a YAML scaffold for documenting metrics, just prior to running
#' [create_metrics()]. You can place this into the header of an Rmd and
#' fill in the names and descriptions.
#'
#' @param tbl A wide metric tbl, including `date`, `period`, optionally one or
#' more dimensions, and one or more calculated metrics.
#'
#' @examples
#'
#' library(dplyr)
#'
#' flights <- nycflights13::flights %>%
#'   mutate(date = as.Date(ISOdate(year, month, day)))
#'
#' # find flight delays by week, month, and quarter
#' flight_summary <- flights %>%
#'   cross_by_dimensions(origin) %>%
#'   cross_by_periods() %>%
#'   summarize(
#'     nb_flights = n(),
#'     avg_arr_delay = mean(arr_delay, na.rm = TRUE)
#'   )
#'
#' use_metrics_scaffold(flight_summary)
#' @export
use_metrics_scaffold <- function(tbl) {
  tbl <- ungroup(tbl)

  names_dimensions <- var_names_dimensions(tbl)
  dimensions <- names_dimensions %>%
    purrr::map(~ list(title = "<TODO>", description = "<TODO>")) %>%
    rlang::set_names(names_dimensions)

  if (length(dimensions) == 0) {
    dimensions <- NULL
  }

  names_metrics <- tbl %>%
    select_if(is.numeric) %>%
    colnames()

  metrics <- names_metrics %>%
    purrr::map(~ list(title = "<TODO>", description = "<TODO>")) %>%
    rlang::set_names(names_metrics)

  out <- list(metrics = metrics, dimensions = dimensions)
  cat(yaml::as.yaml(out))
  invisible(out)
}
