\name{biouml.job.wait}
\alias{biouml.job.wait}
\title{Wait for job completion}
\description{\code{biouml.job.wait} waits for BioUML job completion}
\usage{biouml.job.wait(jobID, verbose=T)}
\arguments{
  \item{jobID}{ID of job usually returned from biouml.analysis}
  \item{verbose}{print messages and progress from BioUML job}
}
\value{
  A list with following entries
  \item{status}{job status, one of 'CREATED','RUNNING', 'PAUSED', 'COMPLETED', 'TERMINATED_BY_REQUEST', 'TERMINATED_BY_ERROR'}
  \item{results}{a list of resulting paths in BioUML repository}
  \item{values}{character vector of messages from job}
  \item{percent}{percent complete}
}
