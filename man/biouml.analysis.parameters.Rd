\name{biouml.analysis.parameters}
\alias{biouml.analysis.parameters}
\title{Get BioUML analysis parameters}
\description{Get BioUML analysis parameters names and description}
\usage{biouml.analysis.parameters(analysisName)}
\arguments{
  \item{analysisName}{name of BioUML analysis, , use \code{\link{biouml.analysis.list}} to get the list of possible values}
}
\value{
  A data frame (\code{\link{data.frame}}) with row.names corresponding to parameter names and one column 'description' with parameter description
}
