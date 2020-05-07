#' Discard dimensions
#'
#' This function discards specified dimensions summary by filtering only for the
#' attribute value "All". If no dimensions are specified, it discards all of them.
#' If the table is grouped, this ungroups it.
#'
#' @param tbl A metric tbl in wide format, with one or more dimensions.
#' @param ... Dimensions to discard.
#' @param quietly If FALSE (default), display a message about what columns are
#' being discarded.
#'
#' @seealso [keep_dimensions()]
#'
#' @export
#' @examples
#' library(dplyr)
#'
#' mtcars_by_cyl_gear <- mtcars %>%
#'   cross_by_dimensions(cyl, gear) %>%
#'   summarize(avg_mpg = mean(mpg))
#'
#' # Discard all dimensions
#' mtcars_by_cyl_gear %>%
#'   discard_dimensions()
#'
#' # Remove dimension cyl
#' mtcars_by_cyl_gear %>%
#'   discard_dimensions(cyl)
#'
#' # Remove all dimensions except `cyl`
#' mtcars_by_cyl_gear %>%
#'   discard_dimensions(-cyl)
#'
#' mtcars_by_cyl_gear %>%
#'   discard_dimensions(-cyl, -gear)
#'
#' mtcars_by_cyl_gear %>%
#'   discard_dimensions(-one_of("cyl", "gear"))
discard_dimensions <- function(tbl, ..., quietly = FALSE){
  cols <- if (rlang::dots_n(...) == 0){
    x <- var_names_dimensions(tbl)
    if (!quietly){
      message("Discarding all dimensions: ", paste(x, collapse = ", "))
    }
    vars(x)
  } else {
    vars_not_dimensions <- var_names_not_dimensions(tbl)
    vars(..., -!!vars_not_dimensions)
  }

  cols_2 <- tbl %>%
    dplyr::ungroup() %>%
    dplyr::select(!!!cols) %>%
    colnames()

  if (length(cols_2) > 0){
    tbl_1 <- tbl %>%
      dplyr::ungroup() %>%
      dplyr::filter_at(cols, all_vars((. == "All")))

    cols_to_remove <- tbl_1 %>%
      # HACK: dates cause errors if not converted to character
      dplyr::mutate_all(as.character) %>%
      dplyr::select_if(~ all(. == "All")) %>%
      colnames()

    tbl_1 %>%
      dplyr::select_at(vars(-one_of(cols_to_remove)))
  } else {
    if(!quietly){
      message("No dimensions left to discard")
    }
    return(tbl)
  }
}


#' Remove attribute "All"
#'
#' This function removes the aggregate segment "All" for specified dimensions.
#' If no dimensions are specified, it removes the segment "All" from all
#' dimensions
#'
#' @param tbl A metric tbl in wide format, with one or more dimensions.
#' @param ... Dimensions from which "All" should be removed, as bare names
#' or select helpers like `contains()`.
#'
#' @export
#' @examples
#' library(dplyr)
#'
#' mtcars_by_cyl_gear <- mtcars %>%
#'   cross_by_dimensions(cyl, gear, vs) %>%
#'   summarize(avg_mpg = mean(mpg))
#'
#' mtcars_by_cyl_gear %>%
#'   remove_attribute_all()
#'
#' mtcars_by_cyl_gear %>%
#'   remove_attribute_all(cyl)
#'
#' mtcars_by_cyl_gear %>%
#'   remove_attribute_all(-cyl, -gear)
remove_attribute_all <- function(tbl, ...){
  cols <- if (rlang::dots_n(...) == 0){
    vars(var_names_dimensions(tbl))
  } else {
    vars_not_dimensions <- var_names_not_dimensions(tbl)
    vars(..., -!!vars_not_dimensions)
  }
  tbl %>%
    filter_at(cols, all_vars((. != "All")))
}

#' Keep dimensions
#'
#' This function keeps specified dimensions from a wide metric tbl and discards the rest.
#'
#' @param tbl A metric tbl in wide format, with one or more dimensions.
#' @param ... Dimensions to keep, as bare names
#' or select helpers like `contains()`.
#' @param keep_attribute_all Whether to remove the "All" level from the dimensions
#' @param quietly If FALSE (default), display a message about what columns are
#' being discarded.
#'
#' @examples
#' library(dplyr)
#'
#' mtcars_by_cyl_gear <- mtcars %>%
#'   cross_by_dimensions(cyl, gear) %>%
#'   summarize(avg_mpg = mean(mpg))
#'
#' mtcars_by_cyl_gear %>%
#'   keep_dimensions()
#'
#' mtcars_by_cyl_gear %>%
#'   keep_dimensions(cyl)
#'
#' mtcars_by_cyl_gear %>%
#'   keep_dimensions(-cyl)
#'
#' mtcars_by_cyl_gear %>%
#'   keep_dimensions(cyl, keep_attribute_all = TRUE)
#'
#' @seealso [discard_dimensions()]
#'
#' @export
keep_dimensions <- function(tbl, ..., keep_attribute_all = FALSE,
                            quietly = FALSE){
  to_keep <- tbl %>%
    ungroup() %>%
    select(...) %>%
    colnames()
  if (length(to_keep) == 0){
    if (!quietly){
      message("Keeping all dimensions")
    }
    if (!keep_attribute_all){
      if (!quietly){
        message("Removing the attribute 'All' from dimensions")
      }
      tbl %>%
        remove_attribute_all()
    } else {
      tbl
    }
  } else {
    tbl_1 <- tbl %>%
      discard_dimensions(-one_of(to_keep))
    if (!keep_attribute_all){
      if (!quietly){
        message("Removing the attribute 'All' from dimensions")
      }
      tbl_1  %>%
        remove_attribute_all()
    } else {
      tbl_1
    }
  }
}

#' Get names of columns that are dimensions
#'
#' Any character of factor column not named date, value, period, or metric are
#' considered dimensions, as well as any columns ending in _id
#'
#' @param tbl A tbl_metric
var_names_dimensions <- function(tbl) {
  set1 <- tbl %>%
    ungroup() %>%
    select_if(~ is.character(.x) || is.factor(.x)) %>%
    colnames() %>%
    setdiff(c('date', 'value', 'period', 'metric'))

  set2 <- stringr::str_subset(colnames(tbl), "_id$")

  union(set1, set2)
}

#' Get names of columns that are NOT dimensions
#'
#' @param tbl A tbl_metric
var_names_not_dimensions <- function(tbl){
  setdiff(colnames(tbl), var_names_dimensions(tbl))
}


#' Remove dimensions with a constant level (single value)
#'
#' Use `constant_constant_dimensions` instead of `select` so the
#' removed dimension value is added to the metadata attribute.
#'
#' @export
#' @param tbl A metric tbl in wide format, with one or more dimensions.
#' @param quietly If FALSE (default), display a message about what columns are
#'   being discarded.
#' @importFrom purrr map keep
#' @examples
#'
#' library(dplyr)
#'
#' flights_nyc_avg_arr_delay %>%
#'   filter(origin == 'JFK') %>%
#'   discard_constant_dimensions()
discard_constant_dimensions <- function(tbl, quietly = FALSE){
  dims <- var_names_dimensions(tbl)
  dims_to_remove <- tbl[dims] %>%
    purrr::map(n_distinct) %>%
    purrr::keep(~ .x == 1) %>%
    names()
  if (length(dims_to_remove) >= 1){
    if (!quietly){
      message("Removing dimensions ", paste(dims_to_remove, collapse = " , "))
    }
    d <- tbl %>%
      select(-one_of(!!!dims_to_remove))
    filters <- attr(d, 'metadata')$dimensions_filters
    attr(d, 'metadata')$dimensions_filters <- append(filters,
      tbl %>%
        as_tibble() %>%
        select(!!!dims_to_remove) %>%
        purrr::map_chr(unique) %>%
        as.list()
    )
    return(d)
  } else {
    tbl
  }
}
