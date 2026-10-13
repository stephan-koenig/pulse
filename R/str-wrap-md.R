#' Wrap Markdown words into formatted paragraphs
#'
#' @description
#' Wrap words into paragraphs based on word length after removing Markdown and
#' HTML markup. Insert line breaks after hyphens, but not within super- or
#' subscripts.
#'
#' @param string Character vector.
#' @param width Positive integer giving target line width (in number of
#'   characters).
#' @param sep String scalar. Will be used to indicate a new line. Default is
#'   `"<br>"` because it will persist as a new line when processed for Markdown.
#'
#' @returns A character vector the same length as string.
#' @export
#' @references
#' Adapted from Navarro, Danielle. 2026.
#' "Markdown Styling in R Plots and Tables." July 3.
str_wrap_md <- function(string, width = 80, sep = "<br>") {
  # Expand last string, start new string when current one is above `width`
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
