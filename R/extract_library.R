#' Extract library annotation
#'
#' This function extracts the relevant information from a screening library package for crispr-nf
#' @importFrom magrittr "%>%"
#' @export

extract_library <- function(df, reverse_complement = FALSE) {

  if (!all(c("id", "gene", "sequence") %in% names(df))) {
    stop("required column names 'id', 'gene', and 'sequence' missing", call. = FALSE)
  }

  lib <- df %>%
    dplyr::select(id, gene, sequence)

  if (isTRUE(reverse_complement)) {
    lib %>%
      dplyr::mutate(sequence = stringi::stri_reverse(chartr("ATGC", "TACG", sequence)))
  } else {
    lib
  }
}
