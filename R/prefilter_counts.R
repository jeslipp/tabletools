#' Filter a count matrix by a minimum value in a selected set of columns
#'
#' This function pre-filters a count matrix before statistical analysis with e.g. MAGeCK
#' @importFrom magrittr "%>%"
#' @export

prefilter_counts <- function(counts, cols, min_count = 50) {

  if (!length(intersect(cols, names(counts))) == length(cols)) {
    stop("`cols` not in column names of count matrix", call. = FALSE)
  }

  if (!all(sapply(counts[, cols], is.numeric))) {
    stop("`cols` contains non-numeric columns", call. = FALSE)
  }

  counts %>%
    dplyr::filter_at(vars(cols), all_vars(. >= min_count))
}
