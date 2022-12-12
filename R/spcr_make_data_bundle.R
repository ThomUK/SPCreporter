#' Make a bundle of data including all SPC calcs
#'
#' @param measure_data list. List containing data frames of data in wide format
#' @param report_config data frame. Config information for the report
#' @param measure_config data frame. Config information for the measures
#'
#' @returns data frame. A nested data frame containing calculated charts and parsed text
#' @export
#'
spcr_make_data_bundle <- function(measure_data = test_measure_data,
                                  report_config = test_report_config,
                                  measure_config = test_measure_config,
                                  data_cutoff_dttm) {

  # check report_config columns and set `ref` column to character
  report_config <- spcr_check_report_config(report_config)

  # check measure_data (list) columns and set `ref` column to character
  measure_data <- spcr_check_measure_data(measure_data)

  # check measure_config columns and set `ref` column to character
  measure_config <- spcr_check_measure_config(measure_config)


  if (rlang::is_missing(data_cutoff_dttm)) {
    data_cutoff_dttm <- Sys.Date() - lubridate::as.period("1s")
    usethis::ui_warn(
      stringr::str_glue("spcr_make_report: Please provide a cutoff date-time.",
            "This is the last time for which data is included in the report.",
            "Using 'yesterday' ({data_cutoff_dttm}) as a default value.",
            collapse = "\n"))
  }

  assertthat::assert_that(
    inherits(data_cutoff_dttm, "POSIXct"),
    msg = "spcr_make_report: The data cutoff date must be a POSIXct object"
  )


  # reduce measure_data list to a single data frame
  measure_data_wide <- measure_data |>
    dplyr::bind_rows(.id = "aggregation")

  # lengthen measure_data
  measure_data_long <- measure_data_wide |>
    spcr_lengthen_measure_data() |>
    dplyr::filter(.data$date <= .env$data_cutoff_dttm)


  # check all required data is supplied
  spcr_check_dataset_is_complete(report_config, measure_data_wide)

  # check reference numbers and measure names agree across both data frames
  # this is to guard against typos and errors in reported figures
  # by ensuring a typo in one place (ref or title) will create an error
  report_config |>
    dplyr::pull("ref") |>
    purrr::walk(spcr_check_measure_names, measure_data_wide, measure_config)



  # measure_data in long format is joined on to the config files as a nested df
  # then mutate the data frame row by row, adding new variables and tidying up
  # or formatting variables ready for reporting
  report_config |>
    dplyr::left_join(measure_config, by = c("ref", "measure_name")) |>
    dplyr::nest_join(measure_data_long,
                     by = c("ref", "aggregation"),
                     name = "measure_data") |>
    dplyr::rowwise() |>
    dplyr::mutate(across(unit, tolower)) |>
    dplyr::mutate(across(improvement_direction, tolower)) |>
    dplyr::mutate(
      target_text = spcr_get_target_text(target, improvement_direction, unit),
      .after = target) |>
    dplyr::mutate(across(target_set_by, ~ tidyr::replace_na(., "-"))) |>
    dplyr::mutate(across(accountable_person, ~ tidyr::replace_na(., "-"))) |>
    dplyr::mutate(across(data_owner, ~ tidyr::replace_na(., "-"))) |>
    dplyr::mutate(across(allowable_days_lag, ~ tidyr::replace_na(., 0))) |>
    dplyr::mutate(across(allowable_days_lag, round)) |>
    dplyr::mutate(first_date = min(measure_data$date), .after = aggregation) |>
    dplyr::mutate(last_date = max(measure_data$date), .after = first_date) |>
    dplyr::mutate(across(c(first_date, last_date), ~ as.Date(., origin = "1970-01-01"))) |>
    dplyr::mutate(last_data_point = dplyr::slice_max(measure_data, date) |> dplyr::pull(value)) |>
    dplyr::mutate(across(last_data_point, ~ dplyr::case_when(
      unit == "%" ~ paste0(round(. * 100, 1), "%"),
      unit == "decimal" ~ as.character(round(., 2)),
      TRUE ~ as.character(round(.))))) |>
    dplyr::mutate(updated_to = spcr_get_updatedto_text(last_date, aggregation)) |>
    dplyr::mutate(stale_data = spcr_calculate_stale_data(
      updated_to, allowable_days_lag, .env$data_cutoff_dttm)) |>
    dplyr::ungroup()
}


make_spc_table <- function(...) {
  p <- rlang::list2(...)

  usethis::ui_info(
    stringr::str_glue("Creating SPC data for ref {p$ref}...")
  )

  if (p$unit == "integer") {
    p$measure_data <- p$measure_data |>
      dplyr::mutate(across(value, round))
  }

  NHSRplotthedots::ptd_spc(
    .data = p$measure_data,
    rebase = spcr_parse_rebase_dates(p$rebase_dates),
    value_field = "value",
    date_field = "date",
    target = p$target,
    improvement_direction = p$improvement_direction
  )
}


make_spc_plot <- function(...) {
  p <- rlang::list2(...)

  usethis::ui_info(
    stringr::str_glue("Creating SPC plot for ref {p$ref}...")
  )

  NHSRplotthedots::ptd_create_ggplot(
    x = p$spc,
    point_size = 4, # default is 2.5, orig here was 5
    percentage_y_axis = p$unit == "%",
    main_title = paste0("#", p$ref, " - ", p$measure_name),
    x_axis_label = NULL,
    y_axis_label = NULL,
    x_axis_date_format = dplyr::if_else(p$aggregation == "week", "%d-%b-%Y", "%b '%y"),
    icons_position = "none",
    break_lines = "limits"
  ) +
    ggplot2::labs(
      caption = paste0("Data source: ", p$data_source)
    ) +
    ggplot2::theme(
      text = ggplot2::element_text(size = 16),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")
    )
}
