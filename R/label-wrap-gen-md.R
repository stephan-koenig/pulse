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

str_wrap_md <- function(string, width = 80, sep = "<br>") {
  expand_last_str_md <- function(strings, string) {
    last_string <- strings[length(strings)]
    initial_strings <- strings[-length(strings)]
    # Join directly after a hyphen within a word, otherwise use a space.
    ends_with_hyphen <- stringr::str_detect(last_string, "(?<! )-$")
    expanded_string <- if (ends_with_hyphen) {
      paste0(last_string, string)
    } else {
      paste(last_string, string)
    }

    if (str_length_md(expanded_string) > width) {
      return(c(strings, string))
    }
    c(initial_strings, expanded_string)
  }

  string |>
    stringr::str_extract_all(
      # Keep base character and super- or subscript as a single token
      "[^\\s-]+<(sup|sub)\\b[^>]*>.*?</\\1>|[^\\s-]*-|[^\\s-]+"
    ) |>
    # Expand last string, start new string when current one is above `width`
    purrr::map(\(string) purrr::reduce(string, expand_last_str_md)) |>
    purrr::map_chr(\(string) paste(string, collapse = sep))
}

str_length_md <- function(string) {
  string |>
    purrr::map_int(\(string) {
      string |>
        # Will add `\n` to end of string
        commonmark::markdown_text() |>
        stringr::str_remove("\n$") |>
        stringr::str_length()
    })
}
