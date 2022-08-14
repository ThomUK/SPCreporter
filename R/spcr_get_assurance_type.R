#' Find the name of the assurance type
#'
#' @param spc A dataframe as returned from the NHSRplothedots SPC package
#' @param improvement_direction A character string of "Increase", "Decrease", or "Neutral"
#'
#' @return String name of the assurance type
#' @noRd
#'
spcr_get_assurance_type <- function(spc, improvement_direction) {

  upl <- spc[1, ]$upl
  lpl <- spc[1, ]$lpl
  target <- spc[1, ]$target

  if(is.na(target) | is.na(lpl) | is.na(upl)) return("N_TARG")

  if(upl > target && target > lpl) return("RND_TARG")

  if(target <= lpl && tolower(improvement_direction) == "increase" ) return("PASS_TARG")
  if(target >= upl && tolower(improvement_direction) == "decrease" ) return("PASS_TARG")

  if(target <= lpl && tolower(improvement_direction) == "decrease" ) return("FAIL_TARG")
  if(target >= upl && tolower(improvement_direction) == "increase" ) return("FAIL_TARG")

  warning("spcr_get_assurance_type: A target has been set for a measure with 'neutral' improvement direction.")
  return("N_TARG")


}
