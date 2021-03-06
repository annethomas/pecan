% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_cf_variables_table.R
\name{build_cf_variables_table_url}
\alias{build_cf_variables_table_url}
\title{Construct a URL to a specific version of the CF variables table XML}
\usage{
build_cf_variables_table_url(version,
  url_format_string = "http://cfconventions.org/Data/cf-standard-names/\%d/src/src-cf-standard-name-table.xml")
}
\arguments{
\item{version}{CF variables table version number (integer/numeric)}

\item{url_format_string}{A format string passed to [sprintf]. This
should contain the entire target URL with the version number
replaced by `"%d"`, and _no other string substitutions_.}
}
\value{
Complete URL, as a string
}
\description{
This uses [sprintf] to construct the URL with the version number as
the first argument.
}
\author{
Alexey Shiklomanov
}
