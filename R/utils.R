#' Gather all the non-dimension columns in a wide metric table
#'
#' @param tbl A table to gather metrics from
#' @param ... A selection of columns to gather. If empty, all numeric columns
#'   are selected as a default
#' @param quietly A boolean indicating if diagnostic messages are to be printed.
#' @importFrom tidyr gather
gather_metrics <- function(tbl, ..., quietly = FALSE) {
  tbl_c <- tbl %>%
    ungroup() %>%
    collect()
  if (rlang::dots_n(...) == 0) {
    cols_numeric <- tbl_c %>%
      select_if(is.numeric) %>%
      colnames() %>%
      grep("\\_id", ., value = TRUE, invert = TRUE)
    if (!quietly) {
      message("Gathering columns ", paste(cols_numeric, collapse = ", "))
    }
    tbl_c %>%
      tidyr::gather(metric, value, cols_numeric)
  } else {
    tbl_c %>%
      tidyr::gather(metric, value, ...)
  }
}
