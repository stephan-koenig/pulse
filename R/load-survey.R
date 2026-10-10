#' Load Qualtrics survey data
#'
#' @description
#' Load data frame of Qualtrics survey data. If no local copy exists, also
#' downloads survey from Qualtrics and saves a local copy.
#'
#' @param base_url A character scalar. Your institution-specific base URL for
#'   your Qualtrics account without `https://`. Find your base URL at
#'   [Base URL and Datacenter IDs | Qualtrics API Docs](https://api.qualtrics.com/1aea264443797-base-url-and-datacenter-i-ds).
#' @param survey_name A character scalar. The survey name on Qualtrics.
#' @param file A character scalar. File to write to or load from.
#' @param api_keyring_id A character scalar. Name of your Qualtrics API token as
#'   saved in your operating system's credential store and accessed with
#'   [keyring]. See
#'   [Generating an API Token | Getting Started with the Qualtrics API](https://www.qualtrics.com/support/integrations/api-integration/overview/#GeneratingAnAPIToken)
#'   for more information. Default is `"qualtrics_api_key"`.
#'
#' @returns A data frame.
#'
#' @export
load_survey <- function(
  base_url,
  survey_name,
  file,
  api_keyring_id = "qualtrics_api_key"
) {
  survey_file <- here::here(file)

  if (fs::file_exists(survey_file)) {
    survey <- qs2::qs_read(survey_file) |>
      janitor::clean_names()
    return(survey)
  }

  api_link <- "Generating an API Token | Getting Started with the Qualtrics API"
  api_url <- paste0(
    "https://www.qualtrics.com/support/integrations/api-integration/",
    "overview/#GeneratingAnAPIToken"
  )
  api_md_link <- glue::glue("[{api_link}]({api_url})")

  cli::cli_alert_info(
    "Accessing your OS credential store for your Qualtrics API token."
  )

  if (api_keyring_id %notin% keyring::key_list()$service) {
    cli::cli_alert_info(
      "No Qualtrics API token is available under {.code {api_keyring_id}}."
    )
    cli::cli_alert_info("You will be prompted to save an API token.")
    cli::cli_text(
      "See {.href {api_md_link}} on how to generate an one."
    )
    keyring::key_set(api_keyring_id)
    cli::cli_alert_success(
      "Qualtrics API token is available under {.code {api_keyring_id}}."
    )
  }
  suppressMessages(
    qualtRics::qualtrics_api_credentials(
      api_key = keyring::key_get(api_keyring_id),
      base_url = base_url
    )
  )

  surveys <- qualtRics::all_surveys()
  survey_id <- surveys$id[surveys$name == survey_name]
  survey <- qualtRics::fetch_survey(
    survey_id,
    breakout_sets = FALSE
  )
  qs2::qs_save(survey, survey_file)
  survey |>
    janitor::clean_names()
}
