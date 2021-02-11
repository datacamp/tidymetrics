#' Given a cumulative metric, add dates for the end of each period
#'
#' Some metrics like ARR are measured cumulatively, so in order to create a bar plot
#' per month or quarter we need to pick the last value from each period. For example,
#' the ARR for January 2019 would be measured as of 2019-01-31. Analogously
#' to the tidyr function `complete()`, this adds rows representing each period
#' present in the data.
#'
#' @param metric A metric table in wide format, containing "date" and "period" columns as
#' well as one or more dimensions and metric values.
#' @param periods Vector of periods to add: one or more of "week", "month", "quarter" or "year".
#' @param add_incomplete If TRUE a value of the running incomplete period will be added.
#'
#' @examples
#'
#' library(dplyr)
#'
#' flights <- nycflights13::flights %>%
#'   mutate(date = as.Date(ISOdate(year, month, day)))
#'
#' # Include number and cumulative number of flights
#' cumulative_summary <- flights %>%
#'   cross_by_periods(periods = "day") %>%
#'   summarize(nb_flights = n()) %>%
#'   arrange(date) %>%
#'   mutate(cumulative_flights = cumsum(nb_flights)) %>%
#'   ungroup()
#'
#' # Have periods for week and month as well, representing the end of that period
#' library(ggplot2)
#'
#' cumulative_day_week_month <- cumulative_summary %>%
#'   complete_periods(periods = c("week", "month"))
#'
#' cumulative_day_week_month %>%
#'   ggplot(aes(date, cumulative_flights, color = period)) +
#'   geom_point()
#' @export
complete_periods <- function(metric, periods = c("month"), add_incomplete = FALSE) {
  # Check the arguments
  if (!"period" %in% colnames(metric)) {
    stop("Metric must have a period column (is this a metric data frame)?")
  }
  if (!"day" %in% metric$period) {
    stop("Metric must have a day period to be completed")
  }
  if (any(!periods %in% c("week", "month", "quarter", "year"))) {
    stop(
      "Only periods that can be added by complete_periods_end are ",
      "week, month, quarter and year"
    )
  }

  # only add periods that aren't already in there
  periods <- setdiff(periods, unique(metric$period))

  # last date we have values for
  last_date <- max(metric$date)

  new_periods <- metric %>%
    dplyr::filter(period == "day") %>%
    dplyr::select(-period) %>%
    tidyr::crossing(period = periods) %>%
    dplyr::group_by(period) %>%
    dplyr::filter(date == as.Date(lubridate::ceiling_date(date, period[1])) - 1 | (add_incomplete & date == last_date)) %>%
    dplyr::mutate(date = as.Date(lubridate::floor_date(date, period[1], week_start = 1))) %>%
    dplyr::ungroup()

  bind_rows(metric, new_periods)
}
