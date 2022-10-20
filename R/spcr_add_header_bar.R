#' Add the header bar to the report
#'
#' @param include_dq_icon logical. Is the data quality icon required on the final report
#'
#' @return string. HTML string for inclusion in the markdown report
#' @noRd
#'
spcr_add_header_bar <- function(include_dq_icon){

  # a function to create the header bar titles
  header_bar <- htmltools::div(
    htmltools::div(
      class = "inner_flex",
      htmltools::div("Vari -ation", class = "spc_logo"),
      htmltools::div("Assur -ance", class = "spc_logo"),
      if(include_dq_icon) htmltools::div("Data Quality", class = "spc_logo")
    ),
    class = "px-10"
  )

  return(htmltools::doRenderTags(header_bar, indent = FALSE))

}
