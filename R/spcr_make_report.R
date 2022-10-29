#' Make the SPC Report
#'
#' @param data_bundle list. The pre-processed bundle of information (made with spcr_make_data_bundle())
#' @param title string. The report title, printed at the top of the report
#' @param subtitle string. The report subtitle, printed at the top of the report
#' @param report_ref string. A unique reference for the report, to make finding it later easier (perhaps the repo name?)
#' @param data_cutoff_dttm POSIXct. The data cutoff date-time (the last date-time for data in the report eg. month-end)
#' @param logo_path string. Filepath of the logo to be used on the report
#' @param department string. A text suffix positioned underneath the logo, for eg. department name
#' @param department_text_colour string. The colour of the department text
#' @param intro string. Intro text printed at the head of the report
#' @param author_name string. The author's name
#' @param author_email string. The author's contact email address
#' @param output_directory string. The name of the directory to save the resulting report to
#' @param paper_colour string. Customise the background colour using a valid HTML Hex Colour Code
#' @param accordion_colour string. Customise the accordion colour using a valid HTML Hex Colour Code
#' @param include_dq_icon logical. Is the data quality icon required on the final report
#'
#' @export
#'
spcr_make_report <- function(data_bundle,
                             title = "SPC Report",
                             subtitle = NULL,
                             report_ref,
                             data_cutoff_dttm,
                             logo_path = NULL,
                             department = NULL,
                             department_text_colour = "black",
                             intro = NULL,
                             author_name,
                             author_email,
                             paper_colour = "#FFFFFF", # white
                             accordion_colour = "#CCF2FF", # pale blue
                             output_directory = "/",
                             include_dq_icon = TRUE) {
  # check that the required arguments are not missing
  # this is necessary because most are used in the Rmd, and do not throw an error here
  purrr::map(
    c(report_ref, data_cutoff_dttm, author_name, author_email),
    exists
  )

  # create the output file name from the report title and a timestamp
  time_stamp <- format.Date(Sys.time(), format = "%Y%m%d_%H%M%S")
  output_file_name <- paste0(
    sub(" ", "_", title), # replace spaces with underscores
    "_",
    time_stamp,
    ".html"
  )

  # render the html output
  message("Making HTML output...")
  rmarkdown::render(
    system.file("Rmd", "Report.Rmd", package = "SPCreporter"),
    output_dir = file.path(getwd(), output_directory),
    output_file = output_file_name
  )
  message("HTML output complete.")

  # print the full path to the console
  path <- file.path("file://", getwd(), output_directory, output_file_name)
  message("Full path: ", path)

  # open the result in the browser
  browseURL(path)

  # finished!!
  beepr::beep(1)
}
