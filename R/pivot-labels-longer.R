#' Pivot data from wide to long creating a column from column labels
#'
#' @description
#' `pivot_labels_longer()` "lengthens" data similar to [tidyr::pivot_longer()],
#' increasing the number of rows and decreasing the number of columns. The
#' column labels are preserved in the column `labels_to`.
#'
#' @inherit tidyr::pivot_longer params
#' @param labels_to A string specifying the new column to
#'   create from the labels of the columns of `data` specified by `cols`.
#' @param names_to A string specifying the new column to create from the
#'   information stored in the column names of `data` specified by `cols`.
#'
#'   - If `NULL` is supplied, no column will be created.
#'   - If length 1, a single column will be created which will contain the
#'     column names specified by `cols`.
#'
#' @returns A data frame.
#'
#' @export
#' @importFrom rlang .data
#' @importFrom rlang :=
# Adapted from Navarro, Danielle. 2026.
# "Markdown Styling in R Plots and Tables." July 3.
# https://blog.djnavarro.net/posts/2026-07-03_subscript/.
pivot_labels_longer <- function(
  data,
  cols,
  ...,
  labels_to = "label",
  names_to = NULL,
  values_to = "value"
) {
  labels_lookup <- extract_labels(data)
  data_long <- data |>
    tidyr::pivot_longer(
      {{ cols }},
      ...,
      # `names_to` can be `NULL`, so first creating `label_to`
      names_to = labels_to,
      values_to = values_to
    )

  if (!is.null(names_to)) {
    data_long <- data_long |>
      dplyr::mutate(
        {{ names_to }} := .data[[labels_to]]
      )
  }
  data_long |>
    dplyr::mutate(
      {{ labels_to }} := .data[[labels_to]] |>
        dplyr::recode_values(
          from = labels_lookup$variable,
          to = labels_lookup$label
        ),
      .before = values_to
    )
}

extract_labels <- function(data) {
  data |>
    purrr::imap(\(x, n) {
      tibble::tibble(
        variable = n,
        label = sjlabelled::get_label(x) %||% NA_character_
      )
    }) |>
    purrr::list_rbind()
}
