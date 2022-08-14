#' Find the name of the variation type
#'
#' @param spc A dataframe as returned from the NHSRplothedots SPC package
#' @param improvement_direction A character string of "Increase", "Decrease", or "Neutral"
#'
#' @return String name of the variation type
#' @noRd
#'
spcr_get_variation_type <- function(spc, improvement_direction) {

  variation <- utils::tail(spc$point_type, n = 1L)
  relative_to_mean <- utils::tail(spc$relative_to_mean, n = 1L)

  if(variation == "common_cause") return("CC")

  if(variation == "special_cause_improvement" && tolower(improvement_direction) == "increase") return("SC_HI_IMP")
  if(variation == "special_cause_improvement" && tolower(improvement_direction) == "decrease") return("SC_LO_IMP")

  if(variation == "special_cause_concern" && tolower(improvement_direction) == "increase") return("SC_LO_CON")
  if(variation == "special_cause_concern" && tolower(improvement_direction) == "decrease") return("SC_HI_CON")

  if(variation == "special_cause_neutral" && relative_to_mean == -1) return("SC_LO_NEUTRAL")
  if(variation == "special_cause_neutral" && (relative_to_mean == 1 | relative_to_mean == 0)) return("SC_HI_NEUTRAL")

  stop("spcr_get_variation_type: Unable to determine SPC variation type.")
}
