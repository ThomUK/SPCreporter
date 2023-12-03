#' Make a bundle of data
#'
#' @param measure_data list. List containing data frames of data in wide format
#' @param report_config data frame. Config information for the report
#' @param measure_config data frame. Config information for the measures
#'
#' @returns data frame. A nested data frame containing source data for the report
#' @export
spcr_make_data_bundle <- function(
    measure_data = test_measure_data,
    report_config = test_report_config,
    measure_config = test_measure_config
    ) {

  # check measure_data (list) columns and set `ref` column to character
  measure_data <- check_measure_data(measure_data)
  # check report_config columns and set `ref` column to character
  report_config <- check_report_config(report_config)
  # check measure_config columns and set `ref` column to character
  measure_config <- check_measure_config(measure_config)

  # measure data can contain two types of worksheet
  # 1. a wide-format sheet containing aggregated counts, with dated columns (a_data)
  # 2. a long-format sheet containing event-list data (e_data).
  # separate them into a_data and e_data
  e_data <- measure_data |>
    purrr::pluck("events")

  a_data <- measure_data
  a_data[["events"]] <- NULL

  # a_data is closely related to the measure_data, but we use a different function to check it
  a_data <- check_a_data(a_data)

  # check event_data columns and set `ref` column to character
  e_data <- check_e_data(e_data)

  # process event data into time-between data
  e_data_time_between <- process_event_data_t(e_data)

  # reduce measure_data list to a single data frame
  a_data_df <- a_data |>
    dplyr::bind_rows(.id = "aggregation")

  # check all required data is supplied
  #TODO reinstate this
#  check_dataset_is_complete(report_config, measure_data_wide)

  # Check reference numbers and measure names agree across both data frames.
  # This is to guard against typos and errors in reported figures
  # by ensuring a typo in one place (ref or title) will raise an error.
  #TODO need to check all of measure_data, not just aggregated a_data
  # report_config |>
  #   dplyr::pull("ref") |>
  #   purrr::walk(\(x) check_measure_names(x, measure_data_wide, measure_config))

  # create long version of the aggregated data, 
  # sorted by date (within each ref), and with 
  # the processed event data added to the end
  measure_data_long <- a_data_df |>
    lengthen_measure_data() |>
    dplyr::bind_rows(e_data_time_between)

  # measure_data in long format is joined on to the config files as a nested df
  # column. Then we mutate the data frame row by row, adding new variables and
  # tidying up / formatting variables ready for reporting
  nested_data <- report_config |>
    # use measure names from report_config not from measure_config
    dplyr::left_join(dplyr::select(measure_config, !"measure_name"), "ref") |>
    dplyr::mutate(
      measure_name = dplyr::case_when(
        spc_chart_type == "t" ~ paste(measure_name, "(time-between)"),
        TRUE ~ measure_name
      )
    ) |>
    dplyr::nest_join(
      measure_data_long,
      by = c("ref", "aggregation"),
      name = "measure_data"
    ) |>

    # pull most recent date from each data frame in the measure_data column
    dplyr::mutate(
      last_date = purrr::map_vec(.data[["measure_data"]], \(x) max(x[["date"]], na.rm = TRUE))
    ) |>
    # pull most recent data point from each data frame in the measure_data column
    dplyr::mutate(
      last_data_point = purrr::map_vec(.data[["measure_data"]], \(x) {
        dplyr::slice_max(x, order_by = x[["date"]], n = 1)[["value"]]
      }
      )
    )

  # Check that measure data that is supposed to be integer data is supplied as
  # such, or raise a warning message
  nested_data |>
    dplyr::filter(if_any("unit", \(x) x == "integer")) |>
    tidyr::hoist("measure_data", "value") |>
    dplyr::select(all_of(c(x = "value", y = "ref"))) |>
    purrr::pwalk(\(x, y) if (any(round(x) != x)) {
      warning(
        glue(
          "spcr_make_data_bundle: ",
          "Measure {y} is configured as an integer, ",
          "but has been supplied with decimal data."
          )
        )
      }
    )

  nested_data |>
    dplyr::mutate(
      across("improvement_direction",
             \(x) dplyr::case_when(
               .data[["spc_chart_type"]] == "t" & x == "decrease" ~ "increase",
               # a rather unlikely situation
               .data[["spc_chart_type"]] == "t" & x == "increase" ~ "decrease",
               TRUE ~ x))
      ) |>
    dplyr::mutate(
      across("unit",
             \(x) if_else(.data[["spc_chart_type"]] == "t", "days", x))
      ) |>
    dplyr::mutate(
      across("target",
             \(x) if_else(.data[["spc_chart_type"]] == "t", NA, x))
      ) |>
    dplyr::mutate(
      across("last_data_point", \(x) dplyr::case_when(
        is.na(x) ~ NA_character_,
        x == Inf ~ NA_character_,
        unit == "%" ~ paste0(round(x * 100, 1), "%"),
        unit == "decimal" ~ as.character(round(x, 2)),
        unit == "days" ~ paste0(x, "d"),
        TRUE ~ as.character(round(x))))
      ) |>
    dplyr::mutate(
      target_text = get_target_text(
        .data[["target"]],
        .data[["improvement_direction"]],
        .data[["unit"]]
        ),
      updated_to = get_updatedto_text(
        .data[["last_date"]],
        .data[["aggregation"]]
        )
      ) |>
    dplyr::mutate(domain_heading = dplyr::row_number() == 1, .by = "domain")
}
