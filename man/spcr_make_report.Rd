% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spcr_make_report.R
\name{spcr_make_report}
\alias{spcr_make_report}
\title{Make the SPC Report}
\usage{
spcr_make_report(
  data_bundle,
  title,
  subtitle = NULL,
  report_ref,
  data_cutoff_dttm,
  logo_path = NULL,
  department = NULL,
  department_text_colour = "black",
  intro = NULL,
  author_name,
  author_email,
  paper_colour = "#FFFFFF",
  accordion_colour = "#CCF2FF",
  output_directory = "/",
  include_dq_icon = TRUE
)
}
\arguments{
\item{data_bundle}{list. The pre-processed bundle of information (made with spcr_make_data_bundle())}

\item{title}{string. The report title, printed at the top of the report}

\item{subtitle}{string. The report subtitle, printed at the top of the report}

\item{report_ref}{string. A unique reference for the report, to make finding it later easier (perhaps the repo name?)}

\item{data_cutoff_dttm}{POSIXct. The data cutoff date-time (the last date-time for data in the report eg. month-end)}

\item{logo_path}{string. Filepath of the logo to be used on the report}

\item{department}{string. A text suffix positioned underneath the logo, for eg. department name}

\item{department_text_colour}{string. The colour of the department text}

\item{intro}{string. Intro text printed at the head of the report}

\item{author_name}{string. The author's name}

\item{author_email}{string. The author's contact email address}

\item{paper_colour}{string. Customise the background colour using a valid HTML Hex Colour Code}

\item{accordion_colour}{string. Customise the accordion colour using a valid HTML Hex Colour Code}

\item{output_directory}{string. The name of the directory to save the resulting report to}

\item{include_dq_icon}{logical. Is the data quality icon required on the final report}
}
\description{
Make the SPC Report
}
