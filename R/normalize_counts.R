#' Apply normalizations to count matrix
#'
#' This function normalizes a count matrix before statistical analysis with e.g. MAGeCK
#' @importFrom magrittr "%>%"
#' @export

normalize_counts <- function(counts, method = "none") {

  if (sum(sapply(counts, is.numeric)) < 2) {
    stop("`counts` contains less than 2 numerical columns", call. = FALSE)
  }

  counts %>%
    dplyr::select_if(is.numeric) %>%
    as.matrix -> mat

  col_names <- colnames(mat)

  if (method == "quantile") {
    mat %>%
      preprocessCore::normalize.quantiles() %>%
      tibble::as_tibble() %>%
      purrr::set_names(col_names) -> df
  }

  is_not_numeric <- function(x) !is.numeric(x)

  counts %>%
    dplyr::select_if(is_not_numeric) %>%
    dplyr::bind_cols(df) %>%
    dplyr::select(names(counts))
}
