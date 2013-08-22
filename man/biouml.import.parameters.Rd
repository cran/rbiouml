\name{biouml.import.parameters}
\alias{biouml.import.parameters}
\title{Get BioUML import parameters}
\description{Get BioUML import parameters}
\usage{biouml.import.parameters(path, importer)}
\arguments{
  \item{path}{path to data element in BioUML repository to import}
  \item{importer}{name of BioUML importer, use \code{\link{biouml.importers}} to get the list of possible values}
}
\value{
  A data frame (\code{\link{data.frame}}) with row.names corresponding to parameter names and one column 'description' with parameter description
}
