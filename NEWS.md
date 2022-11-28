# SPCreporter 0.1.3.9000

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
