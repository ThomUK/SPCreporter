# Add results of reading in example data files, as internal package data.
# This enables the user to run `spcr_make_data_bundle()` using test data
# without needing to read in their own files. This is particularly useful
# for the package developer, as they can rapidly re-test this function after
# making edits, without having to re-read in data repeatedly.


# read in measure data from the two worksheets in the example file
example_files <- here::here("inst", "example_data")
data_file <- here::here(example_files, "data.xlsx")

test_measure_data <- list(
  week = data_file |>
    readxl::read_xlsx(sheet = "week"),
  month = data_file |>
    readxl::read_xlsx(sheet = "month"),
  events = data_file |>
    readxl::read_xlsx(sheet = "events")
  )


# read in the report config from example file
test_report_config <- here::here(example_files, "report_config.xlsx") |>
  readxl::read_xlsx()

# read in measure config from example file
test_measure_config <- here::here(example_files, "measure_config.xlsx") |>
  readxl::read_xlsx()


usethis::use_data(
  test_measure_data,
  test_report_config,
  test_measure_config,
  internal = TRUE,
  overwrite = TRUE
)
