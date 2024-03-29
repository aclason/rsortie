% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CreateNewParameterFiles.R
\name{makeFiles}
\alias{makeFiles}
\title{Create new SORTIE parameter files}
\usage{
makeFiles(
  lstFiles,
  path_basexmls = path_basexmls,
  path_newvals = path_newvals,
  path_newxmls = path_newxmls,
  variable_names = rsortie::VariableNames
)
}
\arguments{
\item{lstFiles}{The name of a text file (string) or a \code{\link[=data.frame]{data.frame()}} that contains all the file names to update and be updated}

\item{path_basexmls}{(optional) the file path to the base parameter file(s) location \code{\link[=character]{character()}}}

\item{path_newvals}{(optional) the file path to the new value file(s) location \code{\link[=character]{character()}}}

\item{path_newxmls}{(optional) the file path to the new output parameter file(s) location \code{\link[=character]{character()}}}

\item{variable_names}{}
}
\value{
This function will generate new .xml files in the \code{path_newxmls} directory, or if \code{path_newxmls}
is not defined, the new .xml will be exported to the working directory.
}
\description{
\code{makeFiles} takes a base SORTIE parameter file (base xmls) and replaces specific
values (new vals) to generate new parameter files (new xmls) to be run in SORTIE.
}
\details{
Common use of this function would be to change the number of timesteps, initiate different
starting stands, or test the effect of changing the values that define a given behaviour (process)

The \code{makeFiles} function requires a table indicating the base SORTIE parameter file (.xml)
and the files containing values to replace in the base parameter file. This table can be read from file, with
\code{lstFiles()} being a string that contains the pathway and file name of the list of files
table (i.e. "pathway/listofmyfiles.csv"). If the list of files is held in a data.frame in the R environment
(as the example), then \code{lstFiles()} requires just the name of that object.

There are two optional pathway arguments available if the required files (base parameter file(s), and new
values file(s)) are not yet present in the R environment. \code{makeFiles()} will read those files in and translate
them into the appropriate format, but requires file directory location be passed by the \code{path_basexml} and
\code{path_newvals} arguments. The \code{path_newxmls} is provided to allow users the ability to organize and store
newly generated SORTIE parameter files in different directories, but if not passed, the new files will
be placed in the working directory.

NOTE - you do not need to assign the makeFiles function to a named object in R, as the output is written to
file, not returned as an object.
}
\examples{
exFiles <- data.frame("type"=c(0,1), "name"=c("samplebasexml","gmf_time_new"))
makeFiles(lstFiles=exFiles)

}
