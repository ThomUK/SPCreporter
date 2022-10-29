#' Returns the markdown used in the Icon Key
#'
#' @return string. Must be valid markdown.
#' @noRd
#'
spcr_icon_key_explanation <- function() {
  return(
    '
**SPC Variation Icons**

Used to summarise the type of variation seen in the most recent data point of a given measure.

Icons | Variation Type |
:----:|:---------------|
<img src = "../img/variation_icons/SC_HI_CON.png" class = "icon"> <img src = "../img/variation_icons/SC_LO_CON.png" class = "icon"> | The most recent data point exhibits special cause variation (in a concerning direction).  H is high, L is Low.
<img src = "../img/variation_icons/SC_HI_IMP.png" class = "icon"> <img src = "../img/variation_icons/SC_LO_IMP.png" class = "icon"> | The most recent data point exhibits special cause variation (in an improving direction).  H is high, L is Low.
<img src = "../img/variation_icons/SC_HI_NEUTRAL.png" class = "icon"> <img src = "../img/variation_icons/SC_LO_NEUTRAL.png" class = "icon"> | The most recent data point exhibits special cause variation, but neither direction represents concern or improvement (ie. the measure is neutral).  H is high, L is low.
<img src = "../img/variation_icons/CC.png" class = "icon"> | The most recent data point exhibits common cause variation (ie. naturally-occurring variation, that is not statistically significant).

 ---

**SPC Assurance Icons**

Used to summarise whether a measure is assured to meet a target.

Icons | Assurance Type
:----:|:---------------
<img src = "../img/assurance_icons/PASS_TARG.png" class = "icon"> | The process is assured, and is likely to consistently pass the target set.
<img src = "../img/assurance_icons/RND_TARG.png" class = "icon"> | The process is not assured, and will pass and fail the target based on variation in the process.
<img src = "../img/assurance_icons/FAIL_TARG.png" class = "icon"> | The process is not assured, and is likely to consistently fail to meet the target set.  '
  )
}
