#' Make Report
#'
#' @param report_title String. The title of the report
#' @param output_directory String. The name of the directory to save the resulting report to
#'
#' @export
#'
spcr_make_report <- function(report_title = "SPC Report", output_directory = "/"){

  # create the output file name from the report title and a timestamp
  time_stamp <- format.Date(Sys.time(), format = "%Y%m%d_%H%M%S")
  output_file_name = paste0(
    sub(" ", "_", report_title), # replace spaces with underscores
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
