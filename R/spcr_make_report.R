#' Make the SPC Report
#'
#' @param data_bundle data frame. The pre-processed bundle of information (made with `spcr_make_data_bundle()`)
#' @param title string. The report title, printed at the top of the report
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
#' @param export_xlsx logical. Whether to export an .xlsx file of the data used in the report, as well as the report itself
#' @param include_dq_icon logical. Is the data quality icon required on the final report?
#'
#' @export
#'
spcr_make_report <- function(data_bundle,
                             title = "SPC Report",
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
                             output_directory = ".",
                             export_xlsx = TRUE,
                             include_dq_icon = TRUE) {

  if (rlang::is_missing(data_cutoff_dttm)) {
    usethis::ui_warn(
      paste("spcr_make_report: Please provide a data cutoff date-time.",
      "This is the last time for which data is included in the report.",
      "Using 'yesterday' as a replacement value.", sep = "\n"))
    data_cutoff_dttm <- Sys.Date() - lubridate::as.period("1s")
  }

  assertthat::assert_that(
    inherits(data_cutoff_dttm, "POSIXct"),
    msg = "spcr_make_report: The data cutoff date must be a POSIXct object"
  )

  # identify stale data and add a flag to the data bundle
  data_bundle <- data_bundle |>
    dplyr::mutate(
      Allowable_Days_Lag = tidyr::replace_na(as.numeric(Allowable_Days_Lag), 0), # allow blanks to come in, but convert them to 0 here
      Stale_Data = spcr_calculate_stale_data(
        as.Date(Updated_To, format = "%d-%b-%Y"),
        Allowable_Days_Lag,
        data_cutoff_dttm)
    ) |>
    dplyr::relocate(Stale_Data, .before = "Needs_Domain_Heading")

  # create the output file name from the report title and a timestamp
  time_stamp <- format.Date(Sys.time(), format = "%Y%m%d_%H%M%S")
  output_file_name <- paste0(
    sub(" ", "_", title), # replace spaces with underscores
    "_",
    time_stamp,
    ".html"
  )

  # create a document title (HTML <title>) unless already supplied
  if (is.null(document_title)) {
    document_title <- paste0(
      title,
      " ",
      format(Sys.Date(), "%d %b %Y")
    )
  }

  # render the html output
  message("Making HTML output...")
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

  # open the result in the browser
  browseURL(path)

  # finished!!
  beepr::beep(1)

  # return the data bundle, which now contains details on stale data
  # return(data_bundle)
}
