\name{biouml.get}
\alias{biouml.get}
\title{Fetch BioUML table}
\description{\code{biouml.get} fetches table data from BioUML server}
\usage{biouml.get(path)}
\arguments{
  \item{path}{Path to table in BioUML repository}
}
\value{
  A data frame (\code{\link{data.frame}}) representation of BioUML table from \code{path}.
}
\examples{\dontrun{
## fetch table from public BioUML server
biouml.login("https://ict.biouml.org")
x <- biouml.get("data/Examples/Optimization/Data/Experiments/exp_data_1")
head(x)
biouml.logout()
}}
