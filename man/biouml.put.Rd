\name{biouml.put}
\alias{biouml.put}
\title{Put data.frame to BioUML repository}
\description{\code{biouml.put} stores data.frame in BioUML repository}
\usage{biouml.put(path, value)}
\arguments{
  \item{path}{Path in BioUML repository}
  \item{value}{data.frame to store}
}
\examples{\dontrun{
## put data.frame into BioUML repository
x <- data.frame(A=1:10,B=LETTERS[1:10])
biouml.login("ie.biouml.org")
biouml.put("data/Collaboration/Demo/Data/Rtest/test_df", x)
}}
