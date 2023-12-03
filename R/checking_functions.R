#' Check the incoming measure data and transform as needed
#'
#' @param measure_data list. A list of data frames containing a combination of aggregated data and event data
#'
#' @returns The input list of data frames, after checking for necessary columns
#' @noRd
check_measure_data <- function(measure_data) {
  assertthat::assert_that(
    inherits(measure_data, "list"),
    msg = "check_measure_data: The data must be a list."
  )

  measure_data <- rlang::set_names(measure_data, tolower)

  assertthat::assert_that(
    any(c("week", "month") %in% names(measure_data)),
    msg = paste0(
      "check_measure_data: ",
      "One element of measure_data must be named 'week' or 'month'"
    )
  )

  # Now we need to only retain data frames from the list if they are named
  # 'week', or 'month'. We then check that each data frame has the
  # required columns and that the 'ref' column is a character type.

  allowed_names <- c(
    "events", "day", "week", "month",
    "calendar_year", "financial_year"
    )
  measure_data |>
    purrr::keep_at(allowed_names) |>
    purrr::iwalk(
      \(x, nm) check_for_required_columns(
        x, nm, required_columns = c("ref", "measure_name", "comment"))
    ) |>
    purrr::map(\(x) dplyr::mutate(x, across("ref", as.character)))
}




#' Check the a_data, which has been split from the incoming measure
#'
#' @param a_data list. A list of data frames (in wide format).
#'
#' @returns The input list of data frames, after checking for necessary columns
#' @noRd
check_a_data <- function(a_data) {
  assertthat::assert_that(
    inherits(a_data, "list"),
    msg = "check_measure_data: The data must be a list."
  )

  # Now we need to only retain data frames from the list if they are named
  # 'week', or 'month'. We then check that each data frame has the
  # required columns and that the 'ref' column is a character type.

  allowed_names <- c(
    "day", "week", "month",
    "calendar_year", "financial_year"
    )
  a_data |>
    purrr::keep_at(allowed_names) |>
    purrr::iwalk(
      \(x, nm) check_for_required_columns(
        x, nm, required_columns = c("ref", "measure_name", "comment"))
    )
}




#' Check the e_data (event data) and transform as needed
#'
#' @param e_data dataframe. A data frames of event data (in long format).
#'
#' @returns The input data frame, after checking for necessary columns
#' @noRd
check_e_data <- function(e_data) {
  assertthat::assert_that(
    inherits(e_data, "data.frame"),
    msg = "check_event_data: The data must be a data frame."
  )

  e_data |>
    check_for_required_columns(
      "events",
      required_columns = c("ref", "measure_name", "comment", "event_date_or_datetime")
    ) |>
    dplyr::mutate(across("ref", as.character))
}




#' Check the report config and transform as needed
#'
#' @param report_config Report config data frame
#'
#' @returns The input data frame after some checks and transformations
#' @noRd
check_report_config <- function(report_config) {
  assertthat::assert_that(
    inherits(report_config, "data.frame"),
    msg = "check_report_config: The report config must be a data frame."
  )

  # check for column names, and provide a helpful error message if needed
  required_columns <- c(
    "ref", "measure_name", "domain", "spc_chart_type", "aggregation"
  )

  optional_columns <- c("report_comment")

  # check required cols are present
  report_config |>
    check_for_required_columns("report_config", required_columns) |>
    check_for_optional_columns(optional_columns) |>
    dplyr::select(c(all_of(required_columns), any_of(optional_columns))) |>
    dplyr::distinct() |>
    dplyr::mutate(across("ref", as.character))
}




#' Check the measure config and transform as needed
#'
#' @param measure_config Measure config data frame
#'
#' @returns The input data frame after some checks and transformations
#' @noRd
check_measure_config <- function(measure_config) {
  assertthat::assert_that(
    inherits(measure_config, "data.frame"),
    msg = "check_measure_config: config_data must be a data frame"
  )

  # check for column names, and provide a helpful error message if needed
  required_columns <- c(
    "ref",
    "measure_name",
    "data_source",
    "data_owner",
    "accountable_person",
    "unit",
    "improvement_direction",
    "target",
    "target_set_by",
    "data_quality",
    # "baseline_period",
    "rebase_dates",
    "rebase_comment"
  )

  optional_columns <- c(
    "allowable_days_lag",
    "reviewed_at",
    "escalated_to"
  )

  measure_config |>
    # check required cols are present
    check_for_required_columns("measure_config", required_columns) |>
    check_for_optional_columns(optional_columns) |>
    dplyr::select(c(all_of(required_columns), any_of(optional_columns))) |>

    dplyr::mutate(
      # default all cols to character (empty cols are imported as logical NAs)
      across(everything(), as.character),
      across("unit", tolower),
      across("improvement_direction", tolower),
      # " marks in the comment mess up the render process later
      across("rebase_comment", \(x) stringr::str_replace_all(x, "\\\"", "'")),
      # target and allowable_days_lag are the only cols that should end up numeric
      across("target", \(x) as.numeric(dplyr::na_if(x, "-"))),
      across("allowable_days_lag", \(x) as.integer(tidyr::replace_na(x, "0")))
    )
}





#' Check measure names to avoid mismatches
#' Returns error message in cases of mismatch
#'
#' @param ref_no A single reference number to check
#' @param measure_data Data frame of measure data in wide format
#' @param measure_config Data frame of config data
#'
#' @returns `TRUE`, invisibly
#' @noRd
check_measure_names <- function(ref_no, measure_data, measure_config) {
  # check that the config table includes this ref_no number
  assertthat::assert_that(
    ref_no %in% measure_config[["ref"]],
    msg = glue(
      "check_measure_names: ",
      "Config data for ref {ref_no} is missing from the measure_config ",
      "data frame.")
  )

  # find the titles to compare
  m_titles <- measure_data |>
    dplyr::filter(if_any("ref", \(x) x == ref_no)) |>
    dplyr::pull("measure_name") |>
    unique()
  c_title <- measure_config |>
    dplyr::filter(if_any("ref", \(x) x == ref_no)) |>
    dplyr::pull("measure_name") |>
    unique()

  assertthat::assert_that(
    length(c_title) == 1,
    msg = glue(
      "check_measure_names: ",
      "There is more than 1 name for measure {ref_no} in the ",
      "measure config."
      )
    )

  # warn when the titles don't match
  m_titles |>
    purrr::walk(
      \(x) ifelse(
        x == c_title,
        usethis::ui_silence(TRUE),
        usethis::ui_warn(
          c(
          "check_measure_names: ",
          "There is a name mismatch for measure ref: {ref_no}.",
          "The title in the data bundle is '{x}'.",
          "The title in the measure config is '{c_title}'."
          )
      )))

  invisible(TRUE)
}






#' Check that required columns are present in a user-supplied data frame
#'
#' @param .data A data frame. The data frame to check
#' @param df_name character. A data frame name to use in the error message
#' @param required_columns character. A vector of the required column names
#'
#' @returns The original data frame, if required columns are present, or an
#'  error message if not
#' @noRd
check_for_required_columns <- function(.data, df_name, required_columns) {

  missing_columns <- setdiff(required_columns, names(.data))

  if (length(missing_columns)) {
    # find the name of the first missing col for the error message
    first_missing_column <- missing_columns[1]

    # throw the error
    usethis::ui_stop(
      "check_for_required_columns: Column '{first_missing_column}' is missing from the '{df_name}' data frame. Check for typos in the column names."
    )
  } else .data
}





#' Certain variables are optional in measure_config. If supplied, we want to
#' keep them, but if not supplied we want to add them with contents = `NA`.
#'
#' @param optional_columns character. A vector of optional column names
#'
#' @returns The original data frame, plus any optional columns that were missing
#' @noRd
check_for_optional_columns <- function(.data, optional_columns) {

  missing_columns <- setdiff(optional_columns, names(.data))
  if (length(missing_columns)) {
    # find the name of the first missing col for the console message
    first_missing_column <- missing_columns[1]

    usethis::ui_info(
      c(
        "check_for_optional_columns: Optional column '{first_missing_column}' is missing. Adding it."
      )
    )
    missing_columns |>
      purrr::reduce(
        \(x, y) tibble::add_column(x, {{y}} := NA_character_),
        .init = .data
      )
  } else .data
}







#' Check all required data items are provided
#'
#' @param report_config A data frame. The report config detailing required report items
#' @param measure_data Data frame in wide format
#'
#' @returns logical TRUE if check is successful, else an error message
check_dataset_is_complete <- function(report_config, measure_data) {

  missing_data <- report_config |>
    dplyr::select(all_of(c("ref", "measure_name", "aggregation"))) |>
    dplyr::filter(!if_any("aggregation", \(x) x == "none")) |>
    dplyr::anti_join(measure_data, by = c("ref", "aggregation"))


  # build an error message if there are missing data items
  assertthat::assert_that(
    nrow(missing_data) == 0,
    msg = usethis::ui_stop(
      dplyr::slice(missing_data, 1) |>
        stringr::str_glue_data(
          "check_dataset_is_complete: ",
          "Data is missing for {nrow(missing_data)} report items. ",
          "The first is ref {ref}, '{measure_name}', {aggregation}ly."
          )
      ))

  invisible(TRUE)
}

