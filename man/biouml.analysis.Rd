\name{biouml.analysis}
\alias{biouml.analysis}
\title{Run BioUML analysis}
\description{Run BioUML analysis optionaly tracking progress}
\usage{biouml.analysis(analysisName, parameters=list(), wait=T, verbose=T)}
\arguments{
  \item{analysisName}{name of BioUML analysis to run}
  \item{parameters}{list of parameters to BioUML analysis}
  \item{wait}{whether to wait for analysis completion or return immediately}
  \item{verbose}{print messages and progress from BioUML analysis, only meaningful if \code{wait} is TRUE}
}
\value{
  Job id that can be passed to \code{\link{biouml.job.info}} and \code{\link{biouml.job.wait}}
}
