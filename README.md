
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

## Installation

You can install the development version of SPCreporter from
[GitHub](https://github.com/ThomUK/SPCreporter) with:

``` r
# install.packages("remotes")
remotes::install_github("ThomUK/SPCreporter")
```

## Examples of Use

#### Concept example - understanding how to use the package

The functions are used to make a report. The first creates a “data
bundle” which contains all the metric-level information needed by the
report.

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
  ... # various report detail arguments, detailed in working example below
)
```

#### Working example - using data included within the package

This example uses three *xlsx* files which are included with the package
in the “example_data” folder. After you have run this example and
created your first report, you should copy these example files to your
machine, and use them as templates for creating your own report
information.

``` r
library(SPCreporter)

# set up to read the package files. You will not need to do this to read your own data.
example_files <- system.file("example_data", package="SPCreporter")

####
# 1. READ THE DATA FILES IN
#############################

# read in measure data from the two worksheets in the example file
measure_data <- list(
  week = readxl::read_xlsx(file.path(example_files, "data.xlsx"), sheet = "week"),
  month = readxl::read_xlsx(file.path(example_files, "data.xlsx"), sheet = "month")
)

# read in the report config from example file
report_config <- readxl::read_xlsx(file.path(example_files, "report_config.xlsx"))

# read in measure config from example file
measure_config <- readxl::read_xlsx(file.path(example_files, "measure_config.xlsx"))

####
# 2. CREATE THE DATA BUNDLE
#############################

# create the data bundle
data_bundle <- spcr_make_data_bundle(
  measure_data = measure_data, 
  report_config = report_config,
  measure_config = measure_config 
)

####
# 3. MAKE THE REPORT
#############################

# make the report, including information for the six mandatory arguments.
spcr_make_report(
  data_bundle = data_bundle,
  report_title = "My Example Report",
  report_ref = "EG.001",
  data_cutoff_dttm = as.POSIXct("2022-09-30 23:59:59"),
  author_name = "Anne Author",
  author_email = "a.author@example.com"
)

# You should now have an example report in your working directory
```

#### Advanced example - creating a suite of reports with `purrr::pwalk()`

``` r
# example where we map over several reports, creating them in one go

all_my_reports <- tibble::tibble(
  report_title = c("Report 1", "Report 2", "Report 3"),
  report_ref = c("ID.1", "ID.2", "ID.3"),
  author_name = "Anne Author",
  author_email = "a.author@example.com",
  data_cutoff_dttm = as.POSIXct("2022-09-30 23:59:59"),
  # create reports with different paper colours
  paper_colour = c("seashell", "thistle", "#afcfaf")
)

# map over the dataframe, which will create 3 separate reports
# in this case with the same dataset...
purrr::pwalk(all_my_reports, spcr_make_report, data_bundle = data_bundle)

# ... but you may want to pass different data to each:
all_my_reports |>
  dplyr::mutate(data_bundle = list(data_bundle1, data_bundle2, data_bundle3)) |>
  purrr::pwalk(spcr_make_report)
```

END
