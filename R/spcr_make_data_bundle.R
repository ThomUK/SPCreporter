#' Make a bundle of data including all SPC calcs
#'
#' @param report_config Dataframe.  Config information for the report
#' @param measure_data List.  List containing dataframes of data in wide format
#' @param measure_config Dataframe.  Config information for the measures
#'
#' @return Dataframe.  A nested dataframe containing calculated charts and parsed text
#' @export
#'
spcr_make_data_bundle <- function(report_config, measure_data, measure_config) {

  #TODO add function to check report_config

  # check measure_data, and lengthen the different aggregation levels into a single long dataframe
  # adding the frequency in as a column
  measure_data <- spcr_check_measure_data(measure_data) %>%
    purrr::map2_df(.y = names(data), .f = spcr_lengthen_measure_data)

  # check measure_config
  measure_config <- spcr_check_measure_config(measure_config)

  # make a vector of the ref numbers to create charts for
  refs <- report_config %>%
    dplyr::pull(ref) %>% unique()

  # check reference numbers and measure names agree across both data frames
  # this is to guard against typos and errors in reported figures
  # by ensuring a typo in one place (ref or title) will create an error
  purrr::walk(refs, spcr_check_measure_titles, measure_data = measure_data, measure_config = measure_config)

  # create and name cols for the results dataframe
  result_df <- as.data.frame(matrix(ncol=23, nrow = length(refs)))
  names(result_df) = c(
    "Ref",
    "Measure_Name",
    "Domain",
    "Aggregation",
    "First_Date",
    "Last_Date",
    "Data_Source",
    "Data_Owner",
    "Lead_Person",
    "Unit",
    "Improvement_Direction",
    "Target",
    "Target_Text",
    "Target_Set_By",
    "Data_Quality",
    "Baseline_Period",
    "Rebase_Dates",
    "Rebase_Comment",
    "Chart_Data",
    "Chart",
    "Last_Data_Point",
    "Variation_Type",
    "Assurance_Type"
  )

  for(i in 1:length(refs)){

    # get the measure ref from the row number
    ref <- refs[i]

    # subset down to the measure of interest
    subset_config <- measure_config[measure_config$ref == ref,]
    subset_report_config <- report_config[report_config$ref == ref,]
    subset_measure_data <- measure_data[measure_data$ref == ref,]

    # separate out the information required
    measure_name <- subset_config$measure_name
    domain <- subset_report_config$domain
    aggregation <- subset_report_config$aggregation
    data_source <- subset_config$data_source
    data_owner <- subset_config$data_owner
    lead_person <- subset_config$lead_person
    unit <- subset_config$unit
    improvement_direction <- subset_config$improvement_direction
    target <- subset_config$target[1]
    target_text <- spcr_get_target_text(target, improvement_direction, unit)
    target_set_by <- subset_config$target_set_by
    data_quality <- subset_config$data_quality
    baseline_period <- subset_config$baseline_period
    rebase_dates <- subset_config$rebase_dates
    rebase_comment <- subset_config$rebase_comment
    first_date <- subset_measure_data$date %>% min()
    last_date <- subset_measure_data$date %>% max()
    last_data_point <- subset_measure_data$value %>% utils::tail(n = 1)

    # friendly formatting for percentages
    if(unit == "%"){
      last_data_point <- paste0(round(last_data_point * 100, 1), "%")
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
    chart_data <- list(list(chart_data = spc))
    chart <- list(list(chart = plot))

    # add helper columns based on spc calculations
    variation_type <- spcr_get_variation_type(spc, improvement_direction)
    assurance_type <- spcr_get_assurance_type(spc, improvement_direction)

    # append the info to the end of the results dataframe
    result_df[i,] <-
      c(
        as.character(ref),
        measure_name,
        domain,
        aggregation,
        first_date,
        last_date,
        data_source,
        data_owner,
        lead_person,
        unit,
        improvement_direction,
        target,
        target_text,
        target_set_by,
        data_quality,
        baseline_period,
        rebase_dates,
        rebase_comment,
        chart_data,
        chart,
        last_data_point,
        variation_type,
        assurance_type
      )

  }

  result_df <- result_df %>%
    dplyr::mutate(
      First_Date = as.Date(result_df$First_Date, origin = "1970-01-01"),
      Last_Date = as.Date(result_df$Last_Date, origin = "1970-01-01")
    )

  return(result_df)

}
