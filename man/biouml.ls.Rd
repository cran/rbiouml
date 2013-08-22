\name{biouml.ls}
\alias{biouml.ls}
\title{List data elements by path}
\description{\code{biouml.ls} lists children data elements by path in BioUML repository}
\usage{biouml.ls(path, extended=F)}
\arguments{
  \item{path}{Path to data collection in BioUML repository}
  \item{extended}{whether to return additional attributes for each children}
}
\value{
  If extended is FALSE a character vector with child names, otherwise a data frame (\code{\link{data.frame}}) with row.names corresponding to child names and columns hasChildren and type.
}
