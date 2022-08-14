#' Add the header bar to the report
#'
#' @noRd
#'
spcr_add_header_bar <- function(){

  # a function to create a mini-card HTML layout
  mini_card <- function(key, value, highlight_colour = "#FFFFFF", class = NULL){
    htmltools::div(
      htmltools::div(key, class = "mini_card_key"),
      htmltools::div(value, class = "mini_card_value", style = paste0("background-color: ", highlight_colour, ";")),
      class = paste0("mini_card ", class)
    )
  }

  # a function to create the header bar titles
  header_bar <- htmltools::div(
    htmltools::div(
      class = "inner_flex",
      htmltools::div(mini_card("Updated to", "", class = "wide_card")),
      htmltools::div(mini_card("Target", "")),
      htmltools::div(mini_card("Set by", "")),
      htmltools::div(mini_card("Actual", "")),
      htmltools::div("Vari -ation", class = "spc_logo"),
      htmltools::div("Assur -ance", class = "spc_logo")
    ),
    class = "px-10"
  )

  return(htmltools::doRenderTags(header_bar, indent = FALSE))

}
