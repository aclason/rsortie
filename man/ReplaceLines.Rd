% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ReplaceFunctions.R
\name{replaceLines}
\alias{replaceLines}
\title{Replace Lines}
\usage{
replaceLines(rf, pf1)
}
\arguments{
\item{rf}{\code{\link[=character]{character()}} Base XML file to be modified}

\item{pf1}{\code{\link[=character]{character()}} Parameter file with new values}
}
\value{

}
\description{
\code{replaceLines()} replaces a chunk of the parameter file with a different set of lines. It assumes the new set of lines are complete and completely replace relevant section.
}
\examples{
xml2 <- replaceLines(xml1, pf1)

}
