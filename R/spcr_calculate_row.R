#' Calculate and format all the information which is later required for
#' the report, including the spc calcs, and the resulting ggplot. This function
#' also handles rounding, capitalisation, and the addition of percentage
#' symbols where appropriate.
#'
#' @param ref character. The reference number for the measure.
#' @param aggregation character. The aggregation level required in the report.
#' @param measure_data dataframe. The data to be reported.
#' @param measure_config dataframe. The config information for each measure.
#' @param report_config dataframe. The config for the report, including domain.
#'
#' @return dataframe. A single row dataframe with chart and spc nested within.
#'
#' @noRd
#'
spcr_calculate_row <- function(ref, aggregation, measure_data, measure_config, report_config){

  # subset down to the measure of interest
  subset_config <- measure_config[measure_config$ref == ref,]
  subset_report_config <- report_config[report_config$ref == ref & report_config$aggregation == aggregation,]
  subset_measure_data <- measure_data[measure_data$ref == ref & measure_data$frequency == aggregation,]

  # separate out the information required
  measure_name <- subset_config$measure_name
  domain <- subset_report_config$domain
  data_source <- subset_config$data_source
  data_owner <- subset_config$data_owner
  lead_person <- subset_config$lead_person
  unit <- subset_config$unit
  improvement_direction <- subset_config$improvement_direction
  target <- subset_config$target[1]
  target_text <- spcr_get_target_text(target, improvement_direction, unit)
  target_set_by <- spcr_get_target_set_by(target, subset_config$target_set_by)
  data_quality <- subset_config$data_quality
  baseline_period <- subset_config$baseline_period
  rebase_dates <- subset_config$rebase_dates
  rebase_comment <- subset_config$rebase_comment
  first_date <- subset_measure_data$date %>% min()
  last_date <- subset_measure_data$date %>% max()
  last_data_point <- subset_measure_data$value %>% utils::tail(n = 1)

  # throw a warning if the unit is "integer", but the data contains decimals
  if(unit == "Integer" & any(subset_measure_data$value%%1 != 0)){
    warning("spcr_calculate_row: Measure ", ref, " is configured as an integer, but has been supplied with decimal data.")
  }

  # friendly formatting for percentages
  if(unit == "%"){
    last_data_point <- paste0(round(last_data_point * 100, 1), "%")
  } else if(unit == "Decimal"){
    last_data_point <- as.character(round(last_data_point, 2))
  } else {
    last_data_point <- as.character(last_data_point)
  }

  is_percentage <- dplyr::if_else(unit == "%", TRUE, FALSE)

  # do the SPC calcs and store the results
  spc <- NHSRplotthedots::ptd_spc(
    subset_measure_data,
    value_field = "value",
    date_field = "date",
    target = target,
    #      fix_after_n_points = #TODO
    improvement_direction = tolower(improvement_direction)
  )
  plot <- spc %>% NHSRplotthedots::ptd_create_ggplot(
    point_size = 3,
    percentage_y_axis = is_percentage,
    main_title = paste0("#", ref, " - ", measure_name),
    x_axis_label = NULL,
    y_axis_label = NULL,
    x_axis_date_format = "%b'%y",
    icons_position = "none",
    break_lines = "limits"
  ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")
    )

  # add helper columns based on spc calculations
  variation_type <- spcr_get_variation_type(spc, improvement_direction)
  assurance_type <- spcr_get_assurance_type(spc, improvement_direction)

  # assemble the result
  result <- tibble::tibble(
    Ref = as.character(ref),
    Measure_Name = measure_name,
    Domain = domain,
    Aggregation = aggregation,
    First_Date = as.Date(first_date, origin = "1970-01-01"),
    Last_Date = as.Date(last_date, origin = "1970-01-01"),
    Data_Source = data_source,
    Data_Owner = data_owner,
    Lead_Person = lead_person,
    Unit = unit,
    Improvement_Direction = improvement_direction,
    Target = target,
    Target_Text = target_text,
    Target_Set_By = target_set_by,
    Data_Quality = data_quality,
    Baseline_Period = baseline_period,
    Rebase_Dates = rebase_dates,
    Rebase_Comment = rebase_comment,
    Chart_Data = list(spc),
    Chart = list(plot),
    Last_Data_Point = last_data_point,
    Variation_Type = variation_type,
    Assurance_Type = assurance_type
  )

}
