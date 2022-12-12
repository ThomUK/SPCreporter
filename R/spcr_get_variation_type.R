#' Find the name of the variation type
#'
#' @param spc data frame. As returned from the {NHSRplotthedots} SPC package
#' @param improvement_direction string. "Increase", "Decrease", or "Neutral"
#'
#' @return string. Name of the variation type
#' @noRd
#'
spcr_get_variation_type <- function(spc, improvement_direction) {
  variation <- tail(spc$point_type, 1)
  relative_to_mean <- tail(spc$relative_to_mean, 1)
  imp_dir <- tolower(improvement_direction)
  v <- NULL

  if (variation == "common_cause") v <- "CC"
  else if (variation == "special_cause_improvement") {
    if (imp_dir == "increase") v <- "SC_HI_IMP"
    if (imp_dir == "decrease") v <- "SC_LO_IMP"
  } else if (variation == "special_cause_concern") {
    if (imp_dir == "increase") v <- "SC_HI_CON"
    if (imp_dir == "decrease") v <- "SC_LO_CON"
  } else if (variation == "special_cause_neutral") {
    if (relative_to_mean == -1) v <- "SC_LO_NEUTRAL"
    if (relative_to_mean %in% c(1, 0)) v <- "SC_HI_NEUTRAL"
  } else if (variation == "special_cause_neutral_low") {
    v <- "SC_LO_NEUTRAL"
  } else if (variation == "special_cause_neutral_high") {
    v <- "SC_HI_NEUTRAL"
  }

  if (is.null(v)) {
    usethis::ui_stop(
      "spcr_get_variation_type: Unable to determine SPC variation type."
    )}
  v
}
