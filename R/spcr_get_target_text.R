#' (internal function) Get the character representation of the target
#'
#' @param target The target (probably a numeric)
#' @param improvement_direction One of "increase", "decrease", or "neutral"
#' @param unit One of "integer", "decimal", or "%"
#'
#' @return A character string suitable for inclusion in the report
#' @noRd
#'
spcr_get_target_text <- function(target, improvement_direction, unit) {

  if(is.na(target)) return("No target set")
  if(tolower(improvement_direction) == "neutral") return("Neutral")

  if(unit == "%") target <- round(target * 100, 1)

  if(tolower(improvement_direction) == "increase") string <- paste0("\u2265 ", target) # \u2265 is: ≥
  if(tolower(improvement_direction) == "decrease") string <- paste0("\u2264 ", target) # \u2264 is: ≤

  if(unit == "%") string <- paste0(string, "%")

  return(string)

}
