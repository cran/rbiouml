\name{biouml.job.info}
\alias{biouml.job.info}
\title{Fetch BioUML job info}
\description{\code{biouml.job.info} fetches info about BioUML job}
\usage{biouml.job.info(jobID)}
\arguments{
  \item{jobID}{ID of job usually returned from biouml.analysis}
}
\value{
  A list with following entries
  \item{status}{job status, one of 'CREATED','RUNNING', 'PAUSED', 'COMPLETED', 'TERMINATED_BY_REQUEST', 'TERMINATED_BY_ERROR'}
  \item{results}{a list of resulting paths in BioUML repository}
  \item{values}{character vector of messages from job}
  \item{percent}{percent complete}
}
