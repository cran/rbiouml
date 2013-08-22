\name{biouml.login}
\alias{biouml.login}
\title{Login to BioUML server}
\description{Login to BioUML server}
\usage{biouml.login(user='', pass='', url='http://localhost:8080/biouml')}
\arguments{
  \item{user}{BioUML user, empty string for anonymous login}
  \item{pass}{password}
  \item{url}{URL of running biouml server}
}
\value{
  Invisible connection to BioUML server, connection is a list with following items:
  \item{user}{}
  \item{pass}{}
  \item{url}{}
  \item{sessionId}{}
}
