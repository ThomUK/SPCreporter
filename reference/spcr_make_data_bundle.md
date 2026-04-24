# Make a bundle of data

Make a bundle of data

## Usage

``` r
spcr_make_data_bundle(
  measure_data = test_measure_data,
  report_config = test_report_config,
  measure_config = test_measure_config,
  data_cutoff_dttm = Sys.time()
)
```

## Arguments

- measure_data:

  list. List containing data frames of data in wide format

- report_config:

  data frame. Config information for the report

- measure_config:

  data frame. Config information for the measures

- data_cutoff_dttm:

  POSIXct. The data cutoff date-time (the last date-time for data in the
  report eg. month-end)

## Value

data frame. A nested data frame containing source data for the report
