#' (Internal function) Create a mini-card HTML layout
#'
#' @param key String. The key name
#' @param value String. The value corresponding to the key
#' @param highlight_colour String. The Hex colour of the background
#' @param class String. The html class name
#'
#' @return
#' @noRd
#'
spcr_mini_card <- function(key, value, highlight_colour = "#FFFFFF", class = NULL){
  htmltools::div(
    htmltools::div(key, class = "mini_card_key"),
    htmltools::div(value, class = "mini_card_value", style = paste0("background-color: ", highlight_colour, ";")),
    class = paste0("mini_card ", class)
  )
}
