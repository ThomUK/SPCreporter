#' Render an accordion unit
#'
#' @param Ref string/numeric. The measure reference
#' @param Measure_Name string. The measure name
#' @param Domain string. The domain the measure relates to. Used for section headings.
#' @param Aggregation string. Aggregation level
#' @param First_Date date.
#' @param Last_Date date.
#' @param Updated_To string. The date of the most recent data (end of reporting period)
#' @param Data_Source string. The name of the data origin
#' @param Data_Owner string. The person/area who manage the reporting data
#' @param Accountable_Person string. The person/area who is accountable for the measure
#' @param Reviewed_At string. The meeting or activity where the measure is reviewed
#' @param Escalated_To string. The meeting or activity where concerning trends are escalated
#' @param Unit string. Integer, Decimal, or %
#' @param Improvement_Direction string. Increase, Decrease, or Neutral
#' @param Target numeric. The target (or NA)
#' @param Target_Text string. A formatted string, complete with rounding
#' @param Target_Set_By string. The name of the person/organisation setting the target
#' @param Data_Quality string. The data quality rating
#' @param Baseline_Period numeric. Number of points over which to set a baseline
#' @param Rebase_Dates string. A list of dates representing rebase dates
#' @param Rebase_Comment string. Commentary on the reason for rebasing
#' @param Chart_Data list. The calculated spc dataframe (from {NHSRplotthedots})
#' @param Chart list. The ggplot chart object
#' @param Last_Data_Point string. The most recent data point
#' @param Variation_Type string. The variation logo name
#' @param Assurance_Type string. The assurance logo name
#' @param Needs_Domain_Heading logical. True if the domain heading should be printed on the report
#' @param accordion_colour string. A valid HTML Hex Colour Code
#' @param include_dq_icon logical. Is the data quality icon required on the final report
#'
#' @return string. HTML string for inclusion in the markdown report
#' @noRd
#'
spcr_render_accordion <- function(Ref,
                                  Measure_Name,
                                  Domain,
                                  Aggregation,
                                  First_Date,
                                  Last_Date,
                                  Updated_To,
                                  Data_Source,
                                  Data_Owner,
                                  Accountable_Person,
                                  Reviewed_At,
                                  Escalated_To,
                                  Unit,
                                  Improvement_Direction,
                                  Target,
                                  Target_Text,
                                  Target_Set_By,
                                  Data_Quality,
                                  Baseline_Period,
                                  Rebase_Dates,
                                  Rebase_Comment,
                                  Chart_Data,
                                  Chart,
                                  Last_Data_Point,
                                  Variation_Type,
                                  Assurance_Type,
                                  Needs_Domain_Heading,
                                  accordion_colour,
                                  include_dq_icon) {
  highlight_colour <- "#FFFFFF" # TODO white for up to date, yellow for delayed updates

  message("Knitting measure: ", Ref, " - ", Measure_Name)

  # handle missing names by replacing with dashes
  if (is.na(Accountable_Person)) Accountable_Person <- "-"
  if (is.na(Data_Owner)) Data_Owner <- "-"

  # prepare spc icons
  if (Variation_Type %in% c("CC", "SC_LO_CON", "SC_LO_NEUTRAL", "SC_LO_IMP", "SC_HI_CON", "SC_HI_NEUTRAL", "SC_HI_IMP")) {
    # render the image
    variation_icon <- htmltools::img(src = system.file("img/variation_icons/", paste0(Variation_Type, ".png"), package = "SPCreporter"), width = "45px")
  } else {
    # pass the text through
    variation_icon <- Variation_Type # text is passed through
  }

  if (Assurance_Type %in% c("PASS_TARG", "FAIL_TARG", "RND_TARG")) {
    # render the image
    assurance_icon <- htmltools::img(src = system.file("img/assurance_icons/", paste0(Assurance_Type, ".png"), package = "SPCreporter"), width = "45px")
  } else {
    # pass the text through
    assurance_icon <- Assurance_Type # the name will be "No SPC", or "No target"
  }

  # prepare data quality assurance indicator icon
  dqai_icon <- htmltools::img(src = system.file("img/dq_icons/", paste0("star_", Data_Quality, ".png"), package = "SPCreporter"), width = "45px") # blank placeholder

  # make html structure for the domain title if required
  domain_title <- if (Needs_Domain_Heading) htmltools::tags$h3(glue::glue("{Domain}:"))

  # make html structure for main accordion
  accordion <- htmltools::tags$details(
    htmltools::tags$summary(
      style = glue::glue(
        "background-color: {accordion_colour}; ",
      ),
      htmltools::div(
        class = "outer_flex",
        htmltools::h4(glue::glue("# {Ref} - {Measure_Name}"), class = "measure_title"),
        htmltools::div(
          class = "inner_flex",
          htmltools::div(spcr_mini_card("Updated to", Updated_To, highlight_colour = highlight_colour, class = "wide_card")), # date of most recent data
          htmltools::div(spcr_mini_card("Target", Target_Text)), # a text string representing the target
          htmltools::div(spcr_mini_card("Set by", Target_Set_By)), # who set the target
          htmltools::div(spcr_mini_card("Actual", Last_Data_Point)), # value for latest date
          htmltools::div(variation_icon, class = "spc_logo"), # spc variation icon
          htmltools::div(assurance_icon, class = "spc_logo"), # spc assurance icon
          if (include_dq_icon) htmltools::div(dqai_icon, class = "spc_logo") # data quality assurance indicator
        )
      )
    ),
    htmltools::div(
      class = "details_content",
      #      htmltools::p(if(!is.null(commentary)) paste0("Commentary: ", commentary)),
      htmltools::plotTag(
        Chart,
        alt = glue::glue("An SPC Chart for metric reference: {Ref}, {Measure_Name}"),
        width = 900,
        height = 450
      ),
      htmltools::p(if (!is.na(Rebase_Comment)) paste0("Rebase comments: ", Rebase_Comment)),
      htmltools::div(paste0("Accountable Person: ", Accountable_Person)),
      htmltools::div(if (!is.na(Reviewed_At)) paste0("Reviewed At: ", Reviewed_At)),
      htmltools::div(if (!is.na(Escalated_To)) paste0("Escalated (if needed) to: ", Escalated_To)),
      htmltools::div(paste0("Data owner: ", Data_Owner))
    ),
    style = glue::glue(
      "margin-bottom: 1rem; "
    ),
  )

  # expand into html
  result <- c(
    cat(htmltools::doRenderTags(domain_title, indent = FALSE)),
    cat(htmltools::doRenderTags(accordion, indent = FALSE))
  )
}
