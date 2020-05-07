#' Generate a table with pairings of dates and periods
#'
#' @param start Start date
#' @param end End date
#' @param periods A vector of calendar periods. This supports "day", "week", "month", "quarter",
#' and "year".
#' @param windows A vector of windows, each representing a # of days
#' @param intervals Whether a preselected set of intervals starting from today, such as
#' "Last Week", "Last 2 Weeks", or "All Time" should be included.
#' @param today_date Date to count as "today": by default, `lubridate::today()`.
generate_date_periods <- function(start,
                                  end,
                                  periods = c("day", "week", "month", "quarter", "year"),
                                  windows = c(7, 28, 56),
                                  intervals = FALSE,
                                  today_date = lubridate::today()) {
  dates <- seq(as.Date(start), as.Date(end), by = 1)

  dates_original <- tibble(date_original = dates)

  ret <- NULL

  if (length(periods) > 0) {
    calendar_periods <- dates_original %>%
      tidyr::crossing(period = periods) %>%
      dplyr::group_by(period) %>%
      dplyr::mutate(
        date = lubridate::floor_date(date_original, period[1], week_start = 1)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(period, date, date_original)

    ret <- bind_rows(ret, calendar_periods)
  }

  if (length(windows) > 0) {
    window_offsets <- tibble::tibble(window_size = windows) %>%
      dplyr::mutate(period = paste0("rolling_", window_size, "d")) %>%
      tidyr::unnest(offset = purrr::map(window_size, seq_len)) %>%
      dplyr::mutate(offset = offset - 1)

    window_periods <- tibble(date_original = dates) %>%
      tidyr::crossing(window_offsets) %>%
      dplyr::mutate(date = date_original + offset) %>%
      dplyr::select(period, date, date_original)

    ret <- bind_rows(ret, window_periods)
  }

  if (intervals) {
    interval_periods <- generate_intervals_ago() %>%
      tibble::enframe("period", "threshold") %>%
      tidyr::crossing(date_original = dates) %>%
      dplyr::filter(date_original < today_date) %>%
      dplyr::filter(date_original >= threshold) %>%
      dplyr::transmute(period, date = NA, date_original)

    ret <- bind_rows(ret, interval_periods)
  }

  if (is.null(ret)) {
    stop("generate_date_periods must be given at least one of periods, intervals, and windows")
  }

  ret
}

generate_intervals_ago <- function(max_date = NULL, today_date = lubridate::today()) {
  weeks_back <- c(1, 2, 4, 8, 12, 26, 365 / 7, 100 * 365 / 7)

  dates <- as.character(today_date - as.integer(weeks_back * 7))

  names(dates) <- dplyr::case_when(
    weeks_back == 1 ~ "Last Week",
    weeks_back <= 12 ~ stringr::str_c("Last ", weeks_back, " Weeks"),
    weeks_back == 26 ~ "Last 6 Months",
    round(weeks_back) == 52 ~ "Last Year",
    weeks_back > 52 ~ "All Time"
  )


  if (!is.null(max_date)) {
    dates <- dates[dates <= max_date]
  }

  dates
}
