#' (internal function) Render an accordion unit
#'
#' @param .data Dataframe. The data bundle
#'
#' @return String. HTML string for inclusion in the markdown report
#' @noRd
#'
spcr_render_accordion <- function(.data){

  background_colour <- "#CCF2FF" # pale blue
  highlight_colour <- "#FFFFFF" #TODO white for up to date, yellow for delayed updates

  message("Knitting measure: ", .data$Ref, " - ", .data$Measure_Name)

  # prepare the latest data string
  if(.data$Aggregation == "week" & !is.na(.data$Last_Date)){
    updated_to <- (lubridate::ceiling_date(.data$Last_Date, unit = "week", week_start = 1) - lubridate::days(1)) %>%
      format.Date("%d-%b-%Y")
  } else if(.data$Aggregation == "month" & !is.na(.data$Last_Date)){
    updated_to <- (lubridate::ceiling_date(.data$Last_Date, unit = "month") - lubridate::days(1)) %>%
      format.Date("%d-%b-%Y")
  } else {
    updated_to <- "-"
  }

  # prepare spc icons
  if(.data$Variation_Type %in% c("CC", "SC_LO_CON", "SC_LO_NEUTRAL", "SC_LO_IMP", "SC_HI_CON", "SC_HI_NEUTRAL", "SC_HI_IMP")) {

    # render the image
    variation_icon <- htmltools::img(src = system.file("img/variation_icons/", paste0(.data$Variation_Type, ".png"), package="SPCreporter"), width = "45px")
  } else{

    # pass the text through
    variation_icon <- .data$Variation_Type # text is passed through
  }

  if(.data$Assurance_Type %in% c("PASS_TARG", "FAIL_TARG", "RND_TARG")) {

    #render the image
    assurance_icon <- htmltools::img(src = system.file("img/assurance_icons/", paste0(.data$Assurance_Type, ".png"), package="SPCreporter"), width = "45px")
  } else{

    # pass the text through
    assurance_icon <- .data$Assurance_Type # the name will be "No SPC", or "No target"
  }

  # prepare data quality assurance indicator icon
  #dqai_icon <- img(src = paste0(dirname(getwd()), "/img/data_quality_assurance_indicator/star_", dq_icon_name, ".png"), width = "45px") # blank placeholder

  #make html structure
  accordion <- htmltools::tags$details(
    htmltools::tags$summary(
      style = glue::glue(
        'background-color: {background_colour}; ',
      ),
      htmltools::div(
        class = "outer_flex",
        htmltools::h4(paste0("#", .data$Ref, " - ", .data$Measure_Name), class = "measure_title"),
        htmltools::div(
          class = "inner_flex",
          htmltools::div(spcr_mini_card("Updated to", updated_to, highlight_colour = highlight_colour, class = "wide_card")), # date of most recent data
          htmltools::div(spcr_mini_card("Target", .data$Target_Text)), # a text string representing the target
          htmltools::div(spcr_mini_card("Set by", .data$Target_Set_By)), # who set the target
          htmltools::div(spcr_mini_card("Actual", .data$Last_Data_Point)), # value for latest date
          htmltools::div(variation_icon, class = "spc_logo"), # spc variation icon
          htmltools::div(assurance_icon, class = "spc_logo"), # spc assurance icon
          #div(dqai_icon, class = "spc_logo") # data quality assurance indicator
        )
      )
    ),
    htmltools::div(
      class = "details_content",
#      htmltools::p(if(!is.null(commentary)) paste0("Commentary: ", commentary)),
      htmltools::plotTag(
        .data$Chart,
        alt = paste0("An SPC Chart for metric reference: ", .data$Ref, ", ", .data$Measure_Name),
        width = 900,
        height = 400
      ),
      htmltools::p(if(nchar(.data$Rebase_Comment)) paste0("Rebase comments: ", .data$Rebase_Comment)),
      htmltools::div(paste0("Metric owner: ", .data$Lead_Person)),
      htmltools::div(paste0("Data owner: ", .data$Data_Owner))
    ),
    style = glue::glue(
      'margin-bottom: 1rem; '
    ),
  )

  #expand into html
  cat(htmltools::doRenderTags(accordion, indent = FALSE))

}
