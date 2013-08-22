\name{biouml.workflow}
\alias{biouml.workflow}
\title{Run BioUML workflow}
\description{Run BioUML workflow optionaly tracking progress}
\usage{biouml.workflow(path, parameters=list(), wait=T, verbose=T)}
\arguments{
  \item{path}{path to BioUML workflow}
  \item{parameters}{list of parameters to BioUML workflow}
  \item{wait}{whether to wait for workflow completion or return immediately}
  \item{verbose}{print messages and progress from BioUML workflow, only meaningful if \code{wait} is TRUE}
}
\value{
  Job id that can be passed to \code{\link{biouml.job.info}} and \code{\link{biouml.job.wait}}
}
