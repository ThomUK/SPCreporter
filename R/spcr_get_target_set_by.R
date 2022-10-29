#' Handle formatting of "target set by" info
#'
#' @param target string/numeric. The target.
#' @param target_set_by string. The organisation who set the target (or NA)
#'
#' @return string. The formatted output string for use on the report
#' @noRd
#'
spcr_get_target_set_by <- function(target, target_set_by) {
  if (is.na(target)) {
    return("-")
  }

  return(target_set_by)
}
