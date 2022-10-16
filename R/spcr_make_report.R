#' Make Report
#'
#' @param data_bundle list. The pre-processed bundle of information (made with spcr_make_data_bundle())
#' @param title string. The report title, printed at the top of the report
#' @param subtitle string. The report subtitle, printed at the top of the report
#' @param intro string. Intro text printed at the head of the report
#' @param output_directory string. The name of the directory to save the resulting report to
#'
#' @export
#'
spcr_make_report <- function(
    data_bundle,
    title = "SPC Report",
    subtitle = NULL,
    intro = NULL,
    output_directory = "/"
  ){

  # add the header bar
  spcr_add_header_bar()

  # create the output file name from the report title and a timestamp
  time_stamp <- format.Date(Sys.time(), format = "%Y%m%d_%H%M%S")
  output_file_name = paste0(
    sub(" ", "_", title), # replace spaces with underscores
    "_",
    time_stamp,
    ".html"
  )

  # render the html output
  message("Making HTML output...")
  rmarkdown::render(
    system.file("Rmd", "Report.Rmd", package="SPCreporter"),
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
