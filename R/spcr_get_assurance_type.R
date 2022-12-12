#' Find the name of the assurance type
#'
#' @param spc data frame. As returned from the {NHSRplotthedots} SPC package
#' @param improvement_direction string.  "Increase", "Decrease", or "Neutral"
#'
#' @return string. Name of the assurance type
#' @noRd
#'
spcr_get_assurance_type <- function(spc, improvement_direction) {
  upl <- spc[1, ]$upl
  lpl <- spc[1, ]$lpl
  target <- spc[1, ]$target

  if (tolower(improvement_direction) == "neutral") {
    return("Neutral")
  }

  if (is.na(target) | is.na(lpl) | is.na(upl)) {
    return("No target")
  }

  if (upl > target && target > lpl) {
    return("RND_TARG")
  }

  if (target <= lpl && tolower(improvement_direction) == "increase") {
    return("PASS_TARG")
  }
  if (target >= upl && tolower(improvement_direction) == "decrease") {
    return("PASS_TARG")
  }

  if (target <= lpl && tolower(improvement_direction) == "decrease") {
    return("FAIL_TARG")
  }
  if (target >= upl && tolower(improvement_direction) == "increase") {
    return("FAIL_TARG")
  }

  stop("spcr_get_assurance_type: Unable to determine SPC assurance type.")
}
