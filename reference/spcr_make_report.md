# Make the SPC Report

Make the SPC Report

## Usage

``` r
spcr_make_report(
  data_bundle,
  report_title = "SPC Report",
  subtitle = NULL,
  document_title = NULL,
  report_ref = "",
  logo_path = "nhs",
  department = NULL,
  department_text_colour = "black",
  intro = NULL,
  author_name = "Anne Author",
  author_email = "a.author@example.com",
  paper_colour = "white",
  accordion_colour = "#CCF2FF",
  stale_colour = "#FFD1AD",
  fresh_colour = "white",
  output_directory = ".",
  output_type = c("html", "csv"),
  include_dq_icon = TRUE,
  annotate_limits = TRUE
)
```

## Arguments

- data_bundle:

  data frame. The pre-processed bundle of information (ideally made with
  [`spcr_make_data_bundle()`](https://thomuk.github.io/SPCreporter/reference/spcr_make_data_bundle.md))

- report_title:

  string. The report title, printed at the top of the report

- subtitle:

  string. The report subtitle, printed at the top of the report

- document_title:

  string. A title for the document, as used in the HTML `<title>` tag or
  as the PDF document title. If left as NULL (the default), this
  function will use the `report_title` parameter and the current date to
  construct a title

- report_ref:

  string. A unique reference for the report

- logo_path:

  string. File path of the logo to be used on the report

- department:

  string. A text suffix positioned underneath the logo, for eg.
  department name

- department_text_colour:

  string. The colour of the department text

- intro:

  string. Intro text printed at the head of the report

- author_name:

  string. The author's name

- author_email:

  string. The author's contact email address

- paper_colour:

  string. Customise the background colour using a hex code, or CSS
  colour name

- accordion_colour:

  string. Customise the accordion colour using a hex code, or CSS colour
  name

- stale_colour:

  string. Customise the date lozenge to indicate that data is stale,
  using a hex code, or CSS colour name

- fresh_colour:

  string. Customise the date lozenge to indicate that data is up to
  date, using a hex code, or CSS colour name

- output_directory:

  string. The name of the directory in which to save the resulting
  report

- output_type:

  vector. Specify what output types are needed. Default is c("html",
  "csv"). "pdf" is also possible.

- include_dq_icon:

  logical. Whether to include the data quality icon on the final report

- annotate_limits:

  logical. Whether to add annotations to a secondary y axis for process
  limits and mean
