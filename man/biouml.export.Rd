\name{biouml.export}
\alias{biouml.export}
\title{Export data from BioUML}
\description{\code{biouml.export} exports data from BioUML server to local file in given format}
\usage{biouml.export(path, exporter="Tab-separated text (*.txt)",
                     exporter.params=list(), target.file="biouml.out")}
\arguments{
  \item{path}{path in BioUML repository}
  \item{exporter}{character string specifying format, \code{\link{biouml.exporters}} provides possible values}
  \item{exporter.params}{list of parameters to exporter}
  \item{target.file}{a character string naming a file to export to}
}
\value{
   None (invisible \code{NULL}).
}
