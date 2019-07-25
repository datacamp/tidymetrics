#' Expand a table so that it can be aggregated by a period
#'
#' Cross by any set of calendar periods (like day or week), rolling windows,
#' or recent intervals (like "4 Weeks", or "8 Weeks"). This means that each
#' row in the input will appear potentially multiple times, each time associated
#' with a different period and date.
#'
#' @param tbl A tbl, either local or remote.
#' @param periods A vector of calendar periods. This supports "day", "week", "month", "quarter",
#' and "year".
#' @param windows A vector of windows, each representing a # of days
#' @param intervals Whether a preselected set of intervals starting from today, such as
#' "Last Week", "Last 2 Weeks", or "All Time" should be included.
#' @param remote_date_periods For crossing remote tables, an existing remote table
#' linking dates to their respective periods. By default, use a global accessor function.
#' @param ... Extra arguments, not used
#'
#' @return A tbl (either local or remote, depending on the input), where TODO. It is grouped by
#' any grouping columns that were in the input, as well as by the new \code{date} and
#' \code{period} columns.
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
#'   cross_by_periods() %>%
#'   summarize(nb_flights = n(),
#'             avg_arr_delay = mean(arr_delay, na.rm = TRUE))
#'
#' library(ggplot2)
#'
#' ggplot(flight_summary, aes(date, avg_arr_delay, color = period)) +
#'   geom_line()
#'
#' @export
cross_by_periods <- function(tbl, periods, windows, intervals, ...) {
  UseMethod("cross_by_periods")
}

#' @rdname cross_by_periods
#' @export
cross_by_periods.tbl_lazy <-  function(tbl,
                                       periods = c("week", "month", "quarter"),
                                       windows = c(),
                                       intervals = FALSE,
                                       remote_date_periods = NULL,
                                       ...) {
  check_cross_by_tbl(tbl)
  # If user provides a vector of intervals, set intervals to TRUE
  # This is required for backward compatibility with the previous version.
  if (!is.logical(intervals) && length(intervals) > 0){
    intervals <- TRUE
  }
  if (is.null(remote_date_periods)) {
    opt <- getOption("remote_date_periods")
    if (is.null(opt)) {
      stop("Can't find option remote_date_periods: have you initialized one for this database?")
    }

    remote_date_periods <- opt()
  }

  all_periods <- c(periods, paste0("rolling_", windows, "d"))

  remote_periods <- remote_date_periods %>%
    filter(
      (period %in% all_periods) |
      (intervals && (period %LIKE% "%All%" || period %LIKE% "%Last%"))
    )

  ## TODO: check that the periods and dates match what's available in the table

  tbl %>%
    rename(date_original = date) %>%
    inner_join(remote_periods, by = "date_original") %>%
    clip_incomplete_rolling_periods() %>%
    group_by(period, date, add = TRUE)
}

clip_incomplete_rolling_periods <- function(tbl){
  # We need to remove incomplete rolling periods at both ends
  # since they could be misleading.
  date_range <- tbl %>%
    ungroup() %>%
    summarize(
      min = min(date_original, na.rm = TRUE),
      max = max(date_original, na.rm = TRUE)
    ) %>%
    collect()

  date_thresholds <- date_range$min + c(7, 28, 56)
  tbl %>%
    mutate(include = case_when(
      period == 'rolling_7d'  ~ date >= !!date_thresholds[1] & date <= !!date_range$max,
      period == 'rolling_28d' ~ date >= !!date_thresholds[2] & date <= !!date_range$max,
      period == 'rolling_56d' ~ date >= !!date_thresholds[3] & date <= !!date_range$max,
      TRUE ~ TRUE
    )) %>%
    filter(include) %>%
    select(-include)
}

#' @rdname cross_by_periods
#' @export
cross_by_periods.tbl_df <-  function(tbl,
                                     periods = c("week", "month", "quarter"),
                                     windows = c(),
                                     intervals = FALSE,
                                     ...) {
  ## TODO:
  ## 1. Update the in-memory version of cross-by-periods to
  ##    follow the same logic as the remote version (clipping, intervals)
  check_cross_by_tbl(tbl)

  date_periods <- generate_date_periods(min(tbl$date),
                                        max(tbl$date),
                                        periods = periods,
                                        windows = windows,
                                        intervals = intervals)

  tbl %>%
    rename(date_original = date) %>%
    inner_join(date_periods, by = "date_original") %>%
    group_by(period, date, add = TRUE)
}

check_cross_by_tbl <- function(tbl) {
  if (!("date" %in% colnames(tbl))) {
    stop("tbl must have a column named \"date\" to be used with cross_by_periods. ",
         "If you have a datetime column, you should cast it to a date first.")
  }
}
