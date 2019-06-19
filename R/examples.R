#' Average arrival delay of NYC flights
#'
#' A tbl_metric showing the average arrival delay of planes departing from
#' an NYC airport over the course of 2013. This can be used as an
#' example of a metric with metadata, and is based on the nycflights13
#' package.
#'
#' @format A tbl_metric object with dimensions:
#' \describe{
#'   \item{origin}{Airport code, either JFK, LGA, or EWR}
#'   \item{carrier}{Two-letter airline code}
#'   \item{period}{Either week, month, or quarter}
#'   \item{date}{Date}
#'   \item{value}{Average arrival delay of a flight in this period}
#' }
#' @source \url{https://cran.r-project.org/package=nycflights13}
"flights_nyc_avg_arr_delay"
