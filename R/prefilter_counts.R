#' Filter a count matrix by a minimum value in a selected set of columns
#'
#' This function pre-filters a count matrix for statistical analysis with e.g. MAGeCK
#' @importFrom magrittr "%>%"
#' @export

prefilter_counts <- function(file, cols, min_count = 50, id_col = "id") {

  if (!file.exists(file)) {
    stop("`file` does not exist", call. = FALSE)
  }

  counts <- readr::read_tsv(file)

  if (!id_col %in% names(counts)) {
    stop("`id_col` not in column names of count matrix", call. = FALSE)
  }
  if (!length(intersect(cols, names(counts))) == length(cols)) {
    stop("`cols` not in column names of count matrix", call. = FALSE)
  }

  id_high <- counts %>%
    dplyr::select_(.dots = c(id_col, cols)) %>%
    tidyr::gather_("sample_name", "count", cols) %>%
    dplyr::group_by_(id_col) %>%
    dplyr::filter(all(count >= min_count)) %>%
    dplyr::ungroup() %>%
    tidyr::spread(sample_name, count) %>%
    dplyr::select(id)

  counts %>%
    dplyr::inner_join(id_high, by = id_col)
}
