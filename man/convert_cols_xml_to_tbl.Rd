% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{convert_cols_xml_to_tbl}
\alias{convert_cols_xml_to_tbl}
\title{Helper function for pulling out information from an xml file using a given xpath}
\usage{
convert_cols_xml_to_tbl(twb_file, twb_xpath)
}
\arguments{
\item{twb_xpath}{the name of the xpath tag}

\item{data}{a TWB file}
}
\value{
returns wanted xpath fields as a tibble
}
\description{
Helper function for pulling out information from an xml file using a given xpath
}
\examples{
 \dontrun{
   convert_cols_xml_to_tbl(twb_file, twb_xpath)
}#'
}
\keyword{internal}
