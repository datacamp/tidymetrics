# A tbl_metric is an S3 class built around a tbl_df, which generally contains a period,
# a date, some number of dimensions, and a value. It supports almost all dplyr operations.

#' S3 operators for metrics, including printing and coercing to a data frame
#'
#' @param x A tbl_metric
#' @param ... Extra arguments, not used.
#'
#' @name metric-methods
#'
#' @import dplyr
#'
#' @export
print.tbl_metric <- function(x, ...) {
  periods <- unique(x$period)
  m <- attr(x, "metadata")

  header <- paste0("# Metric: ", m$title, " (", m$metric_full, ")\n",
                   "# Dimensions: ", paste(var_names_dimensions(x), collapse = ", "), "\n")

  if (!all(is.na(x$date))) {
    header <- paste0(header, "# Dates: ", min(x$date, na.rm = TRUE), " to ", max(x$date, na.rm = TRUE), "\n")
  }
  header <- paste0(header, "# Periods: ", paste(periods, collapse = ", "), "\n")
  if (!is.null(m$updated_at)){
    header <- paste0(header, "# Updated At: ", m$updated_at, "\n")
  }

  cat(pillar::style_subtle(header))

  NextMethod()
}

#' Perform sanity checks on a metric object
#'
#' This function previously worked on metric data tables, but it now works on metric objects
#' (which contain all the metadata, documentation, and everything needed to plot).
#'
#' @param metric A metric table, as found in the data field of a metric object
#'
#' @export
check_metric <- function(metric) {
  assertthat::assert_that(inherits(metric, "tbl_metric"),
                          msg = "Not a 'tbl_metric' object (check_metric parses tbl_metric objects)")

  # Need metric_full to print error messages
  metadata <- attr(metric, "metadata")
  assertthat::assert_that("metric_full" %in% names(metadata),
                          msg = "Missing metric_full field in metric object")

  context_name <- metadata$metric_full

  # check the rest
  expected_names <- c("metric", "title", "description", "category",
                      "subcategory", "owner")

  for (n in expected_names) {
    assertthat::assert_that(n %in% names(metadata),
                            msg = glue::glue("Missing { n } field ({ context_name })"))
  }

  ## check the data
  assertthat::assert_that(inherits(metric, "tbl_df"),
                          msg = glue::glue("Metric data should be a tbl_df ({ context_name })"))
  assertthat::assert_that(nrow(metric) > 0,
                          msg = glue::glue("Metric data should have at least one row ({ context_name })"))

  fields <- colnames(metric)
  fields_numeric <- metric %>%
    select_if(is.numeric) %>%
    colnames()
  fields_dimensions <- var_names_dimensions(metric)

  assertthat::assert_that(
    'date' %in% fields,
    msg = glue::glue("A metric table should have a field named date ({ context_name })")
  )
  assertthat::assert_that(
    'period' %in% fields,
    msg = glue::glue("A metric table should have a field named period ({ context_name })")
  )
  assertthat::assert_that(
    length(fields_numeric) >= 1,
    msg = glue::glue('A metric table should have at least one numeric field  ({ context_name })')
  )


  d <- metadata$dimensions

  # check dimension documentation
  for (dn in names(d)) {
    assertthat::assert_that("title" %in% names(d[[dn]]),
                            msg = glue::glue("Missing title in dimension { dn } ({ context_name })"))
    assertthat::assert_that("description" %in% names(d[[dn]]),
                            msg = glue::glue("Missing title in dimension { dn } ({ context_name })"))
  }
}

#' Condense a metric_tbl object to remove cases with multiple non-All dimensions
#'
#' This reduces the size of a metrics table, by limiting the number of dimensions
#' that can be anything besides All at the same time. If there is a \code{min_dimensions}
#' field in the metric metadata, it never condenses beyond that (this is useful for some
#' that need multiple dimensions to be interpretable)
#'
#' @param metric A \code{tbl_metric} object
#' @param max_dimensions The number of (non-All) dimensions that each row
#' can have
#'
#' @export
condense_metric <- function(metric, max_dimensions = 2) {
  min_dimensions <- attr(metric, "metadata")$min_dimensions
  if (!is.null(min_dimensions)) {
    max_dimensions <- max(min_dimensions, max_dimensions)
  }

  dims <- var_names_dimensions(metric)
  dimensions <- as.matrix(metric[, dims])
  num_not_all <- rowSums(dimensions != "All")

  ret <- metric[num_not_all <= max_dimensions, ]

  # If it's a tbl_metric, keep it that way
  class(ret) <- class(metric)
  attr(ret, "metadata") <- attr(metric, "metadata")
  ret
}


### S3 methods

as_tbl_metric <- function(x) {
  class(x) <- c("tbl_metric", class(x))
  x
}

#' Metric dplyr S3 methods
#'
#' @param .data A \code{tbl_metric} object
#' @param x For as_data_frame, the
#' @param ... Arguments passed on to the appropriate dplyr metric
#'
#' @importFrom dplyr as_tibble anti_join arrange filter group_by inner_join
#' left_join mutate rename right_join select semi_join summarise
#' transmute
#'
#' @name metric-s3
#' @export
as_tibble.tbl_metric <- function(x) {
  class(x) <- class(x)[class(x) != "tbl_metric"]
  x
}

#' Copy class and attributes from the original version of an object to a modified version.
#'
#' Copied over from https://github.com/tidyverse/dplyr/issues/719
#' @param x The original object, which has a class/attributes to copy
#' @param result The modified object, which is / might be missing the
#'   class/attributes.
#'
#' @return \code{result}, now with class/attributes restored.
reclass <- function(x, result) {
  UseMethod('reclass')
}

reclass.tbl_metric <- function(x, result) {
  class(result) <- unique(c(class(x)[[1]], class(result)))
  attr(result, class(x)[[1]]) <- attr(x, class(x)[[1]])
  attr(result, 'metadata') <- attr(x, 'metadata')
  result
}

reclass.tbl_metric_group <- reclass.tbl_metric

#' @rdname metric-s3
#' @importFrom dplyr filter
#' @export
filter.tbl_metric <- function(.data, ...) {
  reclass(.data, NextMethod())
}

registerS3method("filter", "tbl_metric", filter.tbl_metric)

#' @rdname metric-s3
#' @export
select.tbl_metric <- function(.data, ...) {
  reclass(.data, NextMethod())
}

#' @rdname metric-s3
#' @export
arrange.tbl_metric <- function(.data, ...) {
  reclass(.data, NextMethod())
}

#' @rdname metric-s3
#' @export
mutate.tbl_metric <- function(.data, ...) {
  reclass(.data, NextMethod())
}

#' @rdname metric-s3
#' @export
group_by.tbl_metric <- function(.data, ...) {
  reclass(.data, NextMethod())
}

#' @rdname metric-s3
#' @export
summarise.tbl_metric <- function(.data, ...) {
  reclass(.data, NextMethod())
}

#' @rdname metric-s3
#' @export
inner_join.tbl_metric <- function(.data, ...) {
  reclass(.data, NextMethod())
}

#' @rdname metric-s3
#' @export
left_join.tbl_metric <- function(.data, ...) {
  reclass(.data, NextMethod())
}

#' @rdname metric-s3
#' @export
right_join.tbl_metric <- function(.data, ...) {
  reclass(.data, NextMethod())
}

#' @rdname metric-s3
#' @export
semi_join.tbl_metric <- function(.data, ...) {
  reclass(.data, NextMethod())
}

#' @rdname metric-s3
#' @export
anti_join.tbl_metric <- function(.data, ...) {
  reclass(.data, NextMethod())
}

#' @rdname metric-s3
#' @export
rename.tbl_metric <- function(.data, ...) {
  reclass(.data, NextMethod())
}

#' @rdname metric-s3
#' @export
transmute.tbl_metric <- function(.data, ...) {
  reclass(.data, NextMethod())
}


#' @rdname metric-s3
#' @export
distinct.tbl_metric <- function(.data, ...){
  .data <- tibble::as_data_frame(.data)
  NextMethod()
}

# utilities
prune_dimensions <- function(metric){
  metadata <- attr(metric, 'metadata')
  names_dimensions <- intersect(
    names(metadata$dimensions),
    colnames(metric)
  )
  metadata$dimensions <- metadata$dimensions[names_dimensions]
  attr(metric, 'metadata') <- metadata
  return(metric)
}
