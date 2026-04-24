#' Find the name of the assurance type
#'
#' @param spc data frame. As returned from the {NHSRplotthedots} SPC package
#' @param improvement_direction string. "Increase", "Decrease", or "Neutral"
#'
#' @returns string. Name of the assurance type
#' @noRd
get_assurance_type <- function(spc, improvement_direction) {
  imp_dir <- tolower(improvement_direction)
  upl <- tail(spc[["upl"]], 1)
  lpl <- tail(spc[["lpl"]], 1)
  target <- tail(spc[["target"]], 1)

  a <- dplyr::case_when(
    imp_dir == "neutral" ~ "Neutral",
    is.na(target) | is.na(lpl) | is.na(upl) ~ "No target",
    dplyr::between(target, lpl, upl) ~ "RND_TARG",
    lpl > target & imp_dir == "increase" ~ "PASS_TARG",
    upl < target & imp_dir == "decrease" ~ "PASS_TARG",
    lpl > target & imp_dir == "decrease" ~ "FAIL_TARG",
    upl < target & imp_dir == "increase" ~ "FAIL_TARG",
    TRUE ~ ""
  )

  if (a == "") {
    usethis::ui_stop(
      "get_assurance_type: Unable to determine SPC assurance type."
    )
  }
  a
}


#' Find the name of the variation type
#'
#' @param spc data frame. As returned from the {NHSRplotthedots} SPC package
#' @param improvement_direction string. "Increase", "Decrease", or "Neutral"
#'
#' @return string. Name of the variation type
#' @noRd
#'
get_variation_type <- function(spc, improvement_direction) {
  vari <- tail(spc[["point_type"]], 1)
  relative_to_mean <- tail(spc[["relative_to_mean"]], 1)
  # need to provide a default value so the case_when works
  if (is.null(relative_to_mean)) relative_to_mean <- 0
  imp_dir <- tolower(improvement_direction)

  v <- dplyr::case_when(
    vari == "common_cause" ~ "CC",
    vari == "special_cause_improvement" & imp_dir == "increase" ~ "SC_HI_IMP",
    vari == "special_cause_improvement" & imp_dir == "decrease" ~ "SC_LO_IMP",
    vari == "special_cause_concern" & imp_dir == "increase" ~ "SC_LO_CON",
    vari == "special_cause_concern" & imp_dir == "decrease" ~ "SC_HI_CON",
    vari == "special_cause_neutral" & relative_to_mean == -1 ~ "SC_LO_NEUTRAL",
    vari == "special_cause_neutral" & relative_to_mean %in% c(1, 0) ~ "SC_HI_NEUTRAL",
    vari == "special_cause_neutral_low" ~ "SC_LO_NEUTRAL",
    vari == "special_cause_neutral_high" ~ "SC_HI_NEUTRAL",
    TRUE ~ ""
  )

  if (v == "") {
    usethis::ui_stop(
      "get_variation_type: Unable to determine SPC variation type."
    )
  }
  v
}
