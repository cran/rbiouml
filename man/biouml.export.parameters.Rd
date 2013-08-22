\name{biouml.export.parameters}
\alias{biouml.export.parameters}
\title{Get BioUML export parameters}
\description{Get BioUML export parameters}
\usage{biouml.export.parameters(path, exporter)}
\arguments{
  \item{path}{path to data element in BioUML repository to export}
  \item{exporter}{name of BioUML exporter, use \code{\link{biouml.exporters}} to get the list of possible values}
}
\value{
  A data frame (\code{\link{data.frame}}) with row.names corresponding to parameter names and one column 'description' with parameter description
}
