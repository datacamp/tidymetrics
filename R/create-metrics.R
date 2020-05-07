#' Given a metric tbl and an Rmd file, turn into a named list of metric objects
#'
#' @param ... One or more metric tables in wide metric format: one column for each metric.
#' @param rmd_file The Rmd file that generated the compact metrics, which has
#' documentation for the metrics and dimensions stored in the YAML front matter.
#' If no Rmd file is given, it uses the currently running one.
#'
#' @return A named list of metric objects. Each of these has both the data and the metadata
#' (documentation, dimensions, owner, etc) to make an interactive visualization.
#'
#' @examples
#'
#' # TODO
#' @export
create_metrics <- function(..., rmd_file = NULL) {
  metrics <- list(...)

  if (length(metrics) == 0) {
    stop("create_metrics takes at least one argument")
  }

  if (length(metrics) > 1) {
    metrics_each <- purrr::map(metrics, create_metrics, rmd_file = rmd_file)
    all_metrics <- do.call(c, metrics_each)

    # A structure check ensuring uniqueness across all objects
    assertthat::assert_that(length(unique(names(all_metrics))) == length(all_metrics),
      msg = "Metrics don't have unique names"
    )

    return(all_metrics)
  }
  # Now there's just one metric dataset, so construct it
  data <- metrics[[1]]

  metric_docs <- get_metric_docs(rmd_file)

  # an Rmd always has same category/subcategory
  category <- metric_docs[[1]]$category
  subcategory <- metric_docs[[1]]$subcategory

  data_nested <- data %>%
    gather_metrics() %>%
    filter(!is.na(value)) %>%
    tidyr::nest_legacy(-metric) %>%
    dplyr::mutate(metric_full = paste(category, subcategory, metric, sep = "_"))

  missing_metrics <- setdiff(data_nested$metric_full, names(metric_docs))
  if (length(missing_metrics) > 0) {
    stop("Couldn't find documentation for metric(s): ", paste(missing_metrics, collapse = ", "))
  }

  metrics_combined <- data_nested %>%
    mutate(
      documentation = metric_docs[metric_full],
      combined = purrr::map2(data, documentation, combine_metric)
    )

  ret <- metrics_combined$combined %>%
    purrr::map(~ {
      attr(.x, "metadata")$updated_at <- Sys.time()
      return(.x)
    })
  names(ret) <- metrics_combined$metric_full

  # sanity and structure checks
  context_name <- paste(category, subcategory, sep = "_")

  assertthat::assert_that(length(ret) > 0,
    msg = "No metrics found ({ context_name })"
  )
  assertthat::assert_that(length(unique(names(ret))) == length(ret),
    msg = "Metrics don't have unique names ({ context_name })"
  )

  for (metric in ret) {
    check_metric(metric)
  }

  purrr::map(ret, prune_dimensions)
}

get_metric_docs <- function(rmd_file = NULL) {
  if (!is.null(rmd_file)) {
    metric_docs <- parse_metrics_header(rmarkdown::yaml_front_matter(rmd_file))
  } else if (length(rmarkdown::metadata) > 0) {
    metric_docs <- parse_metrics_header(rmarkdown::metadata)
  } else {
    # If running in RStudio, get the current document
    rmd_file <- rstudioapi::getActiveDocumentContext()$path

    if (!stringr::str_detect(rmd_file, "\\.Rmd$")) {
      stop(
        "create_metrics must either be given the path to an Rmd file, run in a rendered Rmd, ",
        "or be run in RStudio as part of the Rmd (that is, by pressing CMD-RETURN with your ",
        "cursor in the Rmd, not e.g. copy-pasted into the R terminal)."
      )
    }

    metric_docs <- parse_metrics_header(rmarkdown::yaml_front_matter(rmd_file))
  }
  return(metric_docs)
}


## Internal utility functions for create_metrics

parse_metrics_header <- function(y) {
  name_components <- stringr::str_split(y$name, "_")[[1]]

  shared <- c(
    list(
      category = name_components[2],
      subcategory = name_components[3]
    ),
    y[c("owner", "dimensions")]
  )

  ret <- purrr::map(names(y$metrics), ~ c(
    list(
      metric = .,
      metric_full = paste(name_components[2],
        name_components[3],
        .,
        sep = "_"
      )
    ),
    y$metrics[[.]],
    shared
  ))
  names(ret) <- purrr::map(ret, "metric_full")
  ret
}

combine_metric <- function(data, metadata) {
  # reorder dimensions
  for (dimension_name in names(metadata$dimensions)) {
    levs <- metadata$dimensions[[dimension_name]]$levels

    if (is.list(levs)) {
      # named list of colors. Grab the name within each of the key-value pairs
      levs <- purrr::map_chr(levs, names)
    }

    if (!is.null(levs) && dimension_name %in% colnames(data)) {
      if (any(duplicated(levs))) {
        stop(glue::glue("Duplicated levels in { dimension_name } in { metadata$metric }"))
      }

      data[[dimension_name]] <- forcats::fct_relevel(data[[dimension_name]], c("All", levs))
    }
  }

  class(data) <- c("tbl_metric", class(data))
  attr(data, "metadata") <- metadata

  # condense it if requested
  if (!is.null(metadata$store_dimensions)) {
    data <- condense_metric(data, metadata$store_dimensions)
  }

  data
}
