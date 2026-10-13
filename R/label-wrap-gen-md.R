#' Labeller for Markdown strip labels
#'
#' @description
#' Labeller for formatting Markdown strip labels of facet grids and wraps.
#'
#' @inherit ggplot2::label_wrap_gen params
#' @export
#' @references
#' Adapted from Navarro, Danielle. 2026.
#' "Markdown Styling in R Plots and Tables." July 3.
#' <https://blog.djnavarro.net/posts/2026-07-03_subscript/>.
label_wrap_gen_md <- function(width = 25, multi_line = TRUE) {
  ggplot2::as_labeller(
    \(label) str_wrap_md(label, width = width, sep = "<br>"),
    multi_line = multi_line
  )
}
