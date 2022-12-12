#' Make the SPC Report
#'
#' @param data_bundle data frame. The pre-processed bundle of information (made with `spcr_make_data_bundle()`)
#' @param report_title string. The report title, printed at the top of the report
#' @param subtitle string. The report subtitle, printed at the top of the report
#' @param document_title string. A title for the document, as used in the HTML `<title>` tag or as the PDF document title. If left as NULL (the default), this function will use the `title` parameter and the current date to construct a title
#' @param report_ref string. A unique reference for the report, to make finding it later easier (perhaps the repo name?)
#' @param data_cutoff_dttm POSIXct. The data cutoff date-time (the last date-time for data in the report eg. month-end)
#' @param logo_path string. Filepath of the logo to be used on the report
#' @param department string. A text suffix positioned underneath the logo, for eg. department name
#' @param department_text_colour string. The colour of the department text
#' @param intro string. Intro text printed at the head of the report
#' @param author_name string. The author's name
#' @param author_email string. The author's contact email address
#' @param paper_colour string. Customise the background colour using a valid HTML hex colour code, or CSS colour name
#' @param accordion_colour string. Customise the accordion colour using a valid HTML hex colour code, or CSS colour name
#' @param output_directory string. The name of the directory to save the resulting report to
#' @param export_csv logical. Whether to export to disk a CSV file of the data used in the report, as well as the report itself
#' @param include_dq_icon logical. Is the data quality icon required on the final report?
#'
#' @export
#'
spcr_make_report <- function(measure_data,
                             report_config,
                             measure_config,
                             report_title = "SPC Report",
                             subtitle = NULL,
                             document_title = NULL,
                             report_ref = "",
                             data_cutoff_dttm,
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
                             export_csv = FALSE,
                             include_dq_icon = TRUE) {

  start_time <- Sys.time()

  data_bundle <- spcr_make_data_bundle(
    measure_data,
    report_config,
    measure_config,
    data_cutoff_dttm
  )

  # create a list of spc data frames (data for charts)
  spc_tables <- data_bundle |>
    # dplyr::mutate(across(target, ~ tidyr::replace_na(., NULL))) |>
    purrr::pmap(make_spc_table)

  spc_plots <- data_bundle |>
    dplyr::mutate(spc = spc_tables) |>
    purrr::pmap(make_spc_plot)


  tmp_files <- paste0("tmp_", data_bundle$ref, "_", data_bundle$aggregation, "_") |>
    tempfile(fileext = ".png")


  tmp_files |>
    purrr::walk2(spc_plots, ggplot2::ggsave, width = 1800, height = 900, units = "px", dpi = 144)

  spc_plot_uris <- tmp_files |>
    purrr::map(knitr::image_uri)



  spcr_add_domain_heading <- function(dtf, sort_col = "sort_order") {
    dtf |>
      dplyr::arrange({{ sort_col }}) |>
      dplyr::mutate(Needs_Domain_Heading = c(TRUE, rep(FALSE, nrow(dtf) - 1)))
  }



  # add helper columns based on spc calculations
  data_bundle_full <- data_bundle |>
    dplyr::mutate(variation_type = purrr::map2_chr(
      spc_tables,
      improvement_direction,
      spcr_get_variation_type)) |>
    dplyr::mutate(assurance_type = purrr::map2_chr(
      spc_tables,
      improvement_direction,
      spcr_get_assurance_type)) |>
    dplyr::rename_with( ~ snakecase::to_mixed_case(toupper(.))) |>
    dplyr::mutate(sort_order = dplyr::row_number()) |>
    dplyr::group_by(Domain) |>
    dplyr::group_modify(spcr_add_domain_heading) |>
    dplyr::ungroup() |>
    dplyr::arrange(sort_order) |>
    dplyr::select(!sort_order) |>
    dplyr::relocate(Measure_Data, .after = last_col())



  # create the output file name from the report title and a timestamp
  time_stamp <- format.Date(Sys.time(), format = "%Y%m%d_%H%M%S")
  output_file_name <- paste0(
    sub(" ", "_", report_title), # replace spaces with underscores
    "_",
    time_stamp,
    ".html"
  )

  # create a document title (HTML <title>), unless already supplied
  if (is.null(document_title)) {
    document_title <- paste0(
      report_title,
      " ",
      format(Sys.Date(), "%d %b %Y")
    )
  }

  # render the html output
  usethis::ui_info("Making HTML output...")
  rmarkdown::render(
    system.file("Rmd", "Report.Rmd", package = "SPCreporter"),
    output_dir = file.path(getwd(), output_directory),
    output_file = output_file_name
  )
  usethis::ui_done("HTML output complete.")

  # print the full path to the console
  wd <- getwd() |>
    stringr::str_remove("^\\\\{1}") # if network location, remove an initial '\'
  path <- file.path("file://", wd, output_directory, output_file_name)
  usethis::ui_info(paste0("Full path: ", path))


  process_duration <- lubridate::as.period(Sys.time() - start_time) |>
    round() |>
    tolower()

  usethis::ui_done(
    stringr::str_glue("Process completed in {process_duration}."))


  # open the result in the browser
  utils::browseURL(path)

  # finished!!
  # beepr::beep(1)

  if (export_csv) {
    csv_filename <- paste0(
      sub(" ", "_", report_title), "_", time_stamp, "_data.csv"
    )

    usethis::ui_info(
      stringr::str_glue("Exporting data CSV file to {csv_filename}"))

    data_bundle_full |>
      tidyr::unnest(Measure_Data) |>
      tidyr::pivot_wider(names_from = date) |>
      dplyr::rename(Comment = comment) |>
      readr::write_csv(csv_filename)
  }
}
