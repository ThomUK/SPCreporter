#' Make report title block
#'
#' @param title string. The title for the report
#' @param data_cutoff_dttm POSIXct. The data cutoff date-time (the last date-time for data in the report eg. month-end)
#' @param subtitle string. The subtitle for the report
#' @param logo_path string. Filepath of the logo to be used on the report
#' @param department string. A text suffix positioned underneath the logo, for eg. department name
#'
#' @return string. An HTML string for inclusion in the Rmd report
#' @noRd
#'
spcr_make_report_title_block <- function(
  title, 
  data_cutoff_dttm, 
  subtitle = NULL, 
  logo_path = NULL, 
  department = NULL
){

assertthat::assert_that(
  !rlang::is_missing(data_cutoff_dttm),
  msg = "spcr_make_report_title_block: Please provide a data cutoff date-time. This is the last time for which data is included in the report."
)

assertthat::assert_that(
  inherits(data_cutoff_dttm, "POSIXct"),
  msg = "spcr_make_report_title_block: The data cutoff date must be a POSIXct object"
)

  # handle the logo path
  if(is.null(logo_path)){
    logo <- system.file("img/NHS_logo.jpg", package="SPCreporter")
  } else{
    logo <- logo_path
  }

  # structure the html
  output <- htmltools::div( # header bar
    style ="display: flex; justify-content: space-between; align-items: flex-start; margin: 3rem 0 2rem 0;",
    htmltools::div( # left-hand header (titles)
      style = "display: flex; flex-direction: column; justify-content: flex-start;",
      htmltools::h1(
        class = "title toc-ignore",
        style = "margin: 0",
        title
      ),
      htmltools::h2(
        style = "margin: 1rem 0 0 0",
        subtitle
      ),
      htmltools::h4(
        style = "margin: 1rem 0 0 0",
        paste0("Report creation date-time: ", format.Date(Sys.time(), "%d/%m/%Y, %H:%M %p")) # timestamp based on actual run-time
      ),
      htmltools::h4(
        style = "margin: 1rem 0 0 0",
        paste0("Data cutoff date-time: ", format.Date(data_cutoff_dttm, "%d/%m/%Y, %H:%M %p"))
      )
    ),
    htmltools::div( # right-hand header (logo)
      style = "width: 30%; display: flex; flex-direction: column; justify-content: flex-start; color: mediumorchid; font-weight: bold;",
      htmltools::img(
        src = logo,
        alt = "Logo"
      ),
      htmltools::div(
        style = "text-align: right;",
        department
      )
    )
  )

  # render into html
  cat(htmltools::doRenderTags(output, indent = FALSE))

}
