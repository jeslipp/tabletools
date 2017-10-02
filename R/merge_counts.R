#' Merge specific columns of a count matrix
#'
#' This function merges columns of a count matrix before statistical analysis with e.g. MAGeCK
#' @importFrom magrittr "%>%"
#' @export

merge_counts <- function(x, cols, agg_fun) {

  if (!length(intersect(unlist(cols), names(x))) == length(unlist(cols))) {
    stop("`cols` not in column names of count matrix", call. = FALSE)
  }

  if (!all(sapply(x[, unlist(cols)], is.numeric))) {
    stop("`cols` must be numeric", call. = FALSE)
  }

  mapping <- reshape2::melt(cols) %>%
    purrr::set_names(c("key", "value")) %>%
    dplyr::mutate(key = as.character(key)) %>%
    tibble::as_tibble()

  extract_numeric_part(x) %>%
    dplyr::mutate(rn = as.integer(rownames(x))) %>%
    tidyr::gather(key, count, -rn) %>%
    dplyr::left_join(mapping, by = "key") %>%
    dplyr::mutate(value = dplyr::if_else(is.na(value), key, value)) %>%
    reshape2::dcast(rn ~ value, value.var = "count", agg_fun) %>%
    dplyr::arrange(rn) %>%
    dplyr::select(-rn) -> df

  is_not_numeric <- function(x) !is.numeric(x)

  x %>%
    dplyr::select_if(is_not_numeric) %>%
    dplyr::bind_cols(df)
}
