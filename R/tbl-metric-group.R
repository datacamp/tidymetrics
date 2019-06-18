#' Create a metric group
#'
#' @param tbl A wide table of metrics
#' @param group_name An optional underscore separated string as group_name. If
#'   not specified, the category and subcategory of the metrics in \code{tbl}
#'   are concatenated to form the group_name. Specify a custom group_name only
#'   if you are trying to save multiple metric groups in the same Rmd.
#' @param rmd_file The Rmd file that generated the compact metrics, which has
#' documentation for the metrics and dimensions stored in the YAML front matter.
#' If no Rmd file is given, it uses the currently running one.
#' @export
#' @rdname metric_group
create_metric_group <- function(tbl, group_name = NULL, rmd_file = NULL){
  metric_details <- get_metric_docs(rmd_file = rmd_file)
  if (is.null(group_name)){
    group_name <- names(metric_details)[1] %>%
      stringr::str_split("_") %>%
      magrittr::extract2(1) %>%
      magrittr::extract(1:2) %>%
      paste(collapse = "_")
  }
  cat_subcat = stringr::str_split(group_name, "_")[[1]]
  category = cat_subcat[1]
  subcategory = cat_subcat[2]
  metric_ids <- var_names_not_dimensions(tbl) %>%
    setdiff(c("date", "period")) %>%
    paste(category, subcategory, . , sep = "_")
  metric_details <- metric_details %>%
    rlang::set_names(
      stringr::str_split_fixed(names(.), "_", 3)[,3]
    )
  dimension_details <- metric_details[[1]]$dimensions
  metadata <- list(
    category = category,
    subcategory = subcategory,
    owner = metric_details[[1]]$owner,
    metrics = metric_details,
    dimensions = dimension_details
  )
  attr(tbl, "metadata") <- metadata
  class(tbl) <- c("tbl_metric_group", class(tbl))
  tbl
}

#' Print method for tbl_metric_group
#'
#' @param x A tbl_metric_group object to \code{print}
#' @param ... Additional parameters passed to \code{print}
#' @export
print.tbl_metric_group <- function(x, ...) {
  m <- attr(x, "metadata")

  header <- paste0("# Metric group\n",
                   "# Category: ", m$category, "\n",
                   "# Subcategory: ", m$subcategory, "\n")

  cat(pillar::style_subtle(header))

  NextMethod("print")
}

as_tbl_metric_group <- function(x) {
  class(x) <- c("tbl_metric_group", class(x))
  x
}

#' Metric dplyr S3 methods
#'
#' @param .data A \code{tbl_metric_group} object
#' @param x For \code{as_tibble}, a \code{tbl_metric_group} object
#' @param ... Arguments passed on to the appropriate dplyr verb
#'
#' @importFrom dplyr as_data_frame anti_join arrange filter group_by inner_join
#' left_join mutate rename right_join select semi_join summarise
#' transmute
#'
#' @name metric-group-s3
#' @export
as_tibble.tbl_metric_group <- function(x, ...) {
  class(x) <- class(x)[class(x) != "tbl_metric_group"]
  x
}

#' @rdname metric-group-s3
#' @export
filter.tbl_metric_group <- function(.data, ...) {
  reclass(.data, NextMethod())
}

#' @rdname metric-group-s3
#' @export
select.tbl_metric_group <- function(.data, ...) {
  reclass(.data, NextMethod())
}

#' @rdname metric-group-s3
#' @export
arrange.tbl_metric_group <- function(.data, ...) {
  reclass(.data, NextMethod())
}

#' @rdname metric-group-s3
#' @export
mutate.tbl_metric_group <- function(.data, ...) {
  reclass(.data, NextMethod())
}

#' @rdname metric-group-s3
#' @export
group_by.tbl_metric_group <- function(.data, ...) {
  reclass(.data, NextMethod())
}

#' @rdname metric-group-s3
#' @export
summarise.tbl_metric_group <- function(.data, ...) {
  reclass(.data, NextMethod())
}

#' @rdname metric-group-s3
#' @export
inner_join.tbl_metric_group <- function(.data, ...) {
  reclass(.data, NextMethod())
}

#' @rdname metric-group-s3
#' @export
left_join.tbl_metric_group <- function(.data, ...) {
  reclass(.data, NextMethod())
}

#' @rdname metric-group-s3
#' @export
right_join.tbl_metric_group <- function(.data, ...) {
  reclass(.data, NextMethod())
}

#' @rdname metric-group-s3
#' @export
semi_join.tbl_metric_group <- function(.data, ...) {
  reclass(.data, NextMethod())
}

#' @rdname metric-group-s3
#' @export
anti_join.tbl_metric_group <- function(.data, ...) {
  reclass(.data, NextMethod())
}

#' @rdname metric-group-s3
#' @export
rename.tbl_metric_group <- function(.data, ...) {
  reclass(.data, NextMethod())
}

#' @rdname metric-group-s3
#' @export
count.tbl_metric_group <- function(.data, ...) {
  reclass(.data, NextMethod())
}

#' @rdname metric-group-s3
#' @export
transmute.tbl_metric_group <- function(.data, ...) {
  reclass(.data, NextMethod())
}

