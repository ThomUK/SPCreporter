
<!-- README.md is generated from README.Rmd. Please edit that file -->

# {SPCreporter}

**{SPCreporter}** is a simple way to add value to your performance
reporting using statistical process control.

**Help sort signals from noise, and ensure your leadership are talking
about signals that matter.**

The layout and method are heavily inspired by the NHS England &
Improvement [“Making Data Count”
programme](https://bmjleader.bmj.com/content/5/4/252), which encourages
the use of SPC and time-series charts to replace “red, amber, green”
threshold performance reporting. The SPC calculations are made using the
{NHSRplotthedots} package, from the [NHS-R
Community](https://nhsrcommunity.com/).

This is a link to my [2022 NHS-R
Conference](https://nhsrcommunity.com/events/nhs-r-online-speaker-conference-2022-9th-november-2022/)
talk, where I presented an overview of the package.  
![](https://www.youtube.com/watch?v=fWYehE5U6Vs&t=2862s)

## Installation

You can install the development version of SPCreporter from
[GitHub](https://github.com/ThomUK/SPCreporter) with:

``` r
# install.packages("remotes")
remotes::install_github("ThomUK/SPCreporter", build_vignettes = TRUE)
```

## Package concept

Two main functions are used to make a report.  
The first creates a “data bundle” which contains all the metric-level
information needed by the report. The second takes this bundle as raw
input, and converts it into the html report.

``` r
library(SPCreporter)

# create a data_bundle, using 3 arguments
data_bundle <- spcr_make_data_bundle(
  measure_data, # your data to plot
  report_config, # config for the report (section titles, etc)
  measure_config  # config for the measures (details required by SPC)
)

# pass the bundle into the make_report function
spcr_make_report(
  data_bundle = data_bundle,
  ... # various report arguments - see full docs for details
)
```

### See the vignettes for additional examples:

[Get Started
vignette](https://thomuk.github.io/SPCreporter/articles/get_started.html) -
Start here to produce your first reprot using data that is bundled into
this package.

[Creating multiple
reports](https://thomuk.github.io/SPCreporter/articles/multiple_reports.html) -
An example of how to use {purrr} to automate groups of reports.

------------------------------------------------------------------------

END
