# SPCreporter 0.1.5

## Feature addition

* Rebasing of SPC charts is now supported.  Pass in a date string in "YYYY-MM-DD" format, or multiple date strings.  `'"2020-01-01"'`, or `'"2020-01-01", "2021-05-01"'`
* Stale data is now highlighted in the report.  By default data is highlighted as stale if the report cutoff is after the data update date (as defined by the `updated_to` argument for the data, and the `data_cutoff_dttm` for the report).  Optionally, a column called `allowable_days_lag` can be included in the measure_config.  This can be used to delay the flagging of stale data.  For example using a value of 31 in the `allowable_days_lag` column will enable a measure to be reported a month in arrears without being flagged as stale.  

## Useability

* Fixes an issue where if there are mismatches in measure_name across any of the 3 supplied dataframes (measure_data, report_config, measure_config) the error message throw reported missing data.  `Error: spcr_check_dataset_is_complete: Data is missing for 1 report items. The first is ref 1, M1, monthly.`  The correct and more helpful error in this case is to complain of the name mismatch: `Error: spcr_check_measure_names: There is a name mismatch for measure ref: 1. Check for typos or mismatching refs or data.`
* Improved progress messages during `spcr_make_data_bundle()` to support debugging of incoming data.

## Bugfix

* Fix a bug relating to NAs in the dataset which was causing printed warnings in the final report, as well as blank space in the charts.  The new behaviour strips NAs from all data, meaning that the chart axis starts at the first data point, and ends at the last.  

# SPCreporter 0.1.4

## Feature addition

* Add two new optional columns to the measure_config: "Reviewed At", and "Escalated To".  If present the text in these fields is rendered into the final report below each chart.  

## Bugfix

* Fixed an error caused when measure_data contained NAs.  The package will now tolerate NAs in the data.  

# SPCreporter 0.1.3

## Useability improvements

* Significant improvements to error messages relating to measure_data, report_config, and measure_config.  Error messages now explicitly name any required columns which the user has not supplied.  This is intended to make troubleshooting simpler to do.  
* The data source for metrics is now named in charts as a caption.  
* Other minor useability improvements intended to make working with real-world config documents easier to do.  

# SPCreporter 0.1.2

## Useability improvements

* measure_data requires data for only one (or more) of "week" or "month".
* measure_data tolerates capitalised list (worksheet) names.
* Use the base pipe |> in place of %>%.
* Throw a helpful error if insufficient data items have been provided for a given report.

# SPCreporter 0.1.1

## Bugfix

* Fixes an error occurring when targets are not set for any of the reported measures


# SPCreporter 0.1.0
## Initial Release

* spcr_make_data_bundle()
* spcr_make_report()
