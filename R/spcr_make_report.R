#' Make the SPC Report
#'
#' @param data_bundle data frame. The pre-processed bundle of information (ideally made with `spcr_make_data_bundle()`)
#' @param data_cutoff_dttm POSIXct. The data cutoff date-time (the last date-time for data in the report eg. month-end)
#' @param report_title string. The report title, printed at the top of the report
#' @param subtitle string. The report subtitle, printed at the top of the report
#' @param document_title string. A title for the document, as used in the HTML `<title>` tag or as the PDF document title. If left as NULL (the default), this function will use the `report_title` parameter and the current date to construct a title
#' @param report_ref string. A unique reference for the report
#' @param logo_path string. File path of the logo to be used on the report
#' @param department string. A text suffix positioned underneath the logo, for eg. department name
#' @param department_text_colour string. The colour of the department text
#' @param intro string. Intro text printed at the head of the report
#' @param author_name string. The author's name
#' @param author_email string. The author's contact email address
#' @param paper_colour string. Customise the background colour using a hex code, or CSS colour name
#' @param accordion_colour string. Customise the accordion colour using a hex code, or CSS colour name
#' @param stale_colour string. Customise the date lozenge to indicate that data is stale, using a hex code, or CSS colour name
#' @param fresh_colour string. Customise the date lozenge to indicate that data is up to date, using a hex code, or CSS colour name
#' @param output_directory string. The name of the directory in which to save the resulting report
#' @param output_type vector. Specify what output types are needed.  Default is c("html", "csv")
#' @param include_dq_icon logical. Whether to include the data quality icon on the final report
#' @param annotate_limits logical. Whether to add annotations to a secondary y axis for process limits and mean
#'
#' @export
spcr_make_report <- function(
    data_bundle,
    data_cutoff_dttm,
    report_title = "SPC Report",
    subtitle = NULL,
    document_title = NULL,
    report_ref = "",
    logo_path = "nhs",
    department = NULL,
    department_text_colour = "black",
    intro = NULL,
    author_name = "Anne Author",
    author_email = "a.author@example.com",
    paper_colour = "white",
    accordion_colour = "#CCF2FF", # pale blue
    stale_colour = "#FFD1AD", # light orange
    fresh_colour = "white",
    output_directory = ".",
    output_type = c("html", "csv"),
    include_dq_icon = TRUE,
    annotate_limits = TRUE
  ) {
  start_time <- Sys.time()

  # Create list of source data for SPC charts
  spc_data <- data_bundle |>
    dplyr::select(all_of(c(
      "target",
      "rebase_dates",
      "improvement_direction",
      "measure_data"
    ))) |>
    purrr::pmap(make_spc_data, .progress = "SPC data")

  # Create list of SPC charts
  spc_charts <- data_bundle |>
    dplyr::select(all_of(c(
      "ref",
      "measure_name",
      "data_source",
      "unit",
      "aggregation"
    ))) |>
    dplyr::mutate(spc_data = spc_data) |>
    dplyr::mutate(label_limits = annotate_limits) |>
    purrr::pmap(make_spc_chart, .progress = "SPC charts")


  tmp_files <- data_bundle |>
    dplyr::select(all_of(c(x = "ref", y = "aggregation"))) |>
    purrr::pmap_chr(\(x, y) paste0("tmp_", x, "_", y, "_")) |>
    tempfile(fileext = ".png")

  tmp_files |>
    purrr::walk2(spc_charts, write_chart_to_img)

  spc_chart_uris <- tmp_files |>
    purrr::map_chr(knitr::image_uri)

  data_bundle_full <- data_bundle |>
    dplyr::mutate(spc_data = spc_data) |>
    dplyr::mutate(spc_chart_uri = spc_chart_uris) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      variation_type = get_variation_type(spc_data, .data[["improvement_direction"]]),
      assurance_type = get_assurance_type(spc_data, .data[["improvement_direction"]])
    ) |>
    dplyr::mutate(stale_data = calculate_stale_data(.data[["updated_to"]], .data[["allowable_days_lag"]], data_cutoff_dttm)) |>
    dplyr::ungroup()



  time_stamp <- format.Date(Sys.time(), format = "%Y%m%d_%H%M%S")

  if ("csv" %in% output_type) {
    csv_filename <- paste0(
      gsub(" ", "_", report_title), "_data_", time_stamp, ".csv"
    )
    usethis::ui_info(
      stringr::str_glue("Exporting data CSV file to {csv_filename}")
    )
    data_bundle |>
      tidyr::hoist("measure_data", "comment", .transform = \(x) head(x, 1)) |>
      tidyr::hoist("measure_data", "date") |>
      tidyr::hoist("measure_data", "value") |>
      dplyr::select(!"measure_data") |>
      tidyr::unnest_longer(c("date", "value")) |>
      tidyr::pivot_wider(names_from = "date") |>
      readr::write_csv(csv_filename)
  }


  # create the report output file name from the report title and the timestamp
  output_file_name <- paste0(
    gsub(" ", "_", report_title), "_", time_stamp, ".html"
  )

  # create a document title (HTML <title>), unless already supplied
  # `pagetitle` in YAML/Pandoc
  # https://community.rstudio.com/t/r-markdown-html-output-title/47294
  if (is.null(document_title)) {
    document_title <- paste0(
      report_title, " ", format(Sys.Date(), "%d %b %Y")
    )
  }

  # render the html output
  usethis::ui_info("Making HTML output...")

  rmarkdown::render(
    system.file("Rmd", "Report.Rmd", package = "SPCreporter"),
    output_options = list(
      toc = FALSE,
      self_contained = TRUE,
      fig_caption = FALSE,
      highlight = NULL,
      mathjax = NULL
    ),
    output_dir = file.path(getwd(), output_directory),
    output_file = output_file_name
  )
  usethis::ui_done("HTML output complete.")

  # print the full path to the console
  wd <- getwd() |>
    stringr::str_remove("^\\\\{1}") # if network location, remove an initial '\'
  path <- file.path("file://", wd, output_directory, output_file_name)
  usethis::ui_info(paste0("Full path: ", path))

  # open the result in the browser
  utils::browseURL(path)
  beepr::beep()

  process_duration <- lubridate::as.period(Sys.time() - start_time) |>
    round() |>
    tolower()

  usethis::ui_done("Report generated in {process_duration}.")

  invisible(TRUE)
}



#' Write a ggplot2 chart to a temporary png file (wrapper round `ggsave()`)
#' @noRd
write_chart_to_img <- function(img_file, chart) {
  ggplot2::ggsave(
    filename = img_file,
    plot = chart,
    device = ragg::agg_png,
    width = 1500,
    height = 750,
    units = "px",
    dpi = 125
    )
}


#' Create a 'plot the dots' SPC data parcel from data bundle columns
#' @noRd
make_spc_data <- function(
    target,
    rebase_dates,
    improvement_direction,
    measure_data) {
  measure_data |>
    NHSRplotthedots::ptd_spc(
      rebase = align_rebase_dates(rebase_dates, measure_data),
      value_field = "value",
      date_field = "date",
      target = target,
      improvement_direction = improvement_direction)
}

#' Create an SPC chart from an SPC data parcel and some data bundle columns
#' @noRd
make_spc_chart <- function(
    ref,
    measure_name,
    data_source,
    unit,
    aggregation,
    spc_data,
    label_limits = TRUE) {
  spc_data |>
    NHSRplotthedots::ptd_create_ggplot(
      point_size = 4, # default is 2.5, orig in this package was 5
      percentage_y_axis = unit == "%",
      main_title = paste0("#", ref, " - ", measure_name),
      x_axis_label = NULL,
      y_axis_label = NULL,
      x_axis_date_format = dplyr::if_else(aggregation == "week", "%d-%b-%Y", "%b '%y"),
      label_limits = label_limits,
      icons_position = "none",
      break_lines = "limits"
    ) +
    ggplot2::labs(
      caption = paste0("Data source: ", data_source)
    ) +
    ggplot2::theme(
      text = ggplot2::element_text(size = 16),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")
    )
}

#' Convert HTML output to PDF
#' @param filepath. A file path to the HTML file
#' @noRd
convert_to_pdf <- function(filepath) {
  usethis::ui_info("Making PDF output...")

  out_path <- file.path(tempdir(), basename(filepath))
  pdf_path <- xfun::with_ext(filepath, "pdf")
  filepath |>
    readr::read_file() |>
    stringr::str_replace_all("<details>", "<details open>") |>
    readr::write_file(out_path)

  pagedown::chrome_print(out_path, pdf_path, scale = 2)

  usethis::ui_done("PDF output complete.")
  usethis::ui_info("PDF filepath: {pdf_path}")
  invisible(TRUE)
}
