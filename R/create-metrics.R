#' Given a metric tbl and an Rmd file, turn into a named list of metric objects
#'
#' @param ... One or more metric tables in wide metric format: one column for
#'   each metric.
#' @param rmd_file The Rmd file that generated the compact metrics, which has
#'   documentation for the metrics and dimensions stored in the YAML front
#'   matter. If no Rmd file is given, it uses the currently running one.
#' @param category A string indicating a category for the metric. It overrides
#'   the values in the `rmd_file` and the default values.
#' @param subcategory A string indicating a subcategory for the metric. It
#'   overrides the values in the `rmd_file` and the default values.
#' @param metrics A named list of metrics. Each item in the list should have a
#'   title and a description. It overrides the values in the `rmd_file` and the
#'   defaults values.
#' @param dimensions A named list of dimensions. Each item in the list should
#'   have a title and a description. It overrides the values in the `rmd_file`
#'   and the defaults values.
#' @param owner A string indicating an owner for the metric.  It overrides the
#'   values in the `rmd_file` and the default values.
#' @return A named list of metric objects. Each of these has both the data and
#'   the metadata (documentation, dimensions, owner, etc) to make an interactive
#'   visualization.
#'
#' @examples
#'
#' # TODO
#' @export
create_metrics <- function(...,
                           rmd_file = NULL,
                           category = NULL,
                           subcategory = NULL,
                           metrics = NULL,
                           dimensions = NULL,
                           owner = NULL) {
  # Get documentation
  metric_docs <- get_metric_docs(
    rmd_file,
    category = category,
    subcategory = subcategory, metrics = metrics,
    dimensions = dimensions, owner = owner,
    ...
  )

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

  # an Rmd always has same category/subcategory
  category <- metric_docs[[1]]$category
  subcategory <- metric_docs[[1]]$subcategory

  data_nested <- data %>%
    gather_metrics() %>%
    filter(!is.na(value)) %>%
    tidyr::nest_legacy(-metric) %>%
    dplyr::mutate(metric_full = purrr::map_chr(metric, ~ {
      y <- c(category, subcategory, .x)
      paste(y[y != ""], collapse = "_")
    }))

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

# Get metadata from an Rmd document
get_rmd_metadata <- function(rmd_file = NULL) {
  if (!is.null(rmd_file)) {
    rmarkdown::yaml_front_matter(rmd_file)
  } else if (length(rmarkdown::metadata) > 0) {
    rmarkdown::metadata
  } else {
    rmd_file <- rstudioapi::getActiveDocumentContext()$path
    rmarkdown::yaml_front_matter(rmd_file)
  }
}

# Get metric documentation
get_metric_docs <- function(rmd_file = NULL,
                            category = NULL,
                            subcategory = NULL,
                            owner = NULL,
                            metrics = NULL,
                            dimensions = NULL,
                            ...) {
  y <- get_rmd_metadata(rmd_file)
  `%||%` <- function(x, y) {
    if (is.null(x)) y else x
  }

  name_components <- if (!is.null(y$name)) {
    stringr::str_split(y$name, "_")[[1]]
  } else {
    c()
  }
  shared <- list(
    category = category %||% y$category %||% name_components[2] %||% "",
    subcategory = subcategory %||% y$subcategory %||% name_components[3] %||% "",
    owner = owner %||% y$owner %||% "",
    dimensions = dimensions %||% y$dimensions %||% doc_dimensions(...)
  )
  metrics <- metrics %||% y$metrics %||% doc_metrics(...)

  docs <- names(metrics) %>%
    purrr::map(~ {
      y <- c(shared$category, shared$subcategory, .x)
      c(
        metrics[[.x]],
        shared,
        list(
          metric = .x,
          metric_full = paste(y[y != ""], collapse = "_")
        )
      )
    })
  rlang::set_names(docs, purrr::map(docs, "metric_full"))
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
        stop(glue::glue(
          "Duplicated levels in { dimension_name } in { metadata$metric }"
        ))
      }

      data[[dimension_name]] <- forcats::fct_relevel(
        data[[dimension_name]], c("All", levs)
      )
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
