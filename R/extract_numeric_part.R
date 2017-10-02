#' Extract the numerical part from the count matrix
#'
#' Helper function to extract the numerical part of the matrix
#' @importFrom magrittr "%>%"
#' @export

extract_numeric_part <- function(counts) {

  if (sum(sapply(counts, is.numeric)) < 2) {
    stop("`counts` contains less than 2 numerical columns", call. = FALSE)
  }

  counts %>%
    dplyr::select_if(is.numeric)
}
