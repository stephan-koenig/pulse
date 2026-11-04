#' Split a string into rows
#'
#' `separate_longer_delim_labels()` has the same functionality as
#' [tidyr::separate_longer_delim()] but preserves variable labels.
#'
#' @inheritParams tidyr::separate_longer_delim
#' @inherit tidyr::separate_longer_delim returns
#'
#' @export
separate_longer_delim_preserving_labels <- function(data, ...) {
  tidyr::separate_longer_delim(data, ...) |>
    labelled::copy_labels_from(data)
}
