#' Create, modify, and delete columns while preserving variable labels
#'
#' `mutate_preserving_labels()` has the same functionality as
#' [dplyr::mutate()] but preserves variable labels.
#'
#' @inheritParams dplyr::mutate
#' @inherit dplyr::mutate returns
#' @export
mutate_preserving_labels <- function(.data, ...) {
  dplyr::mutate(.data, ...) |>
    labelled::copy_labels_from(.data)
}
