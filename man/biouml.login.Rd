\name{biouml.login}
\alias{biouml.login}
\title{Login to BioUML server}
\description{Login to BioUML server. The connection will be saved in global options under name biouml_connection for future reuse.}
\usage{biouml.login(url='http://localhost:8080/biouml', user='', pass='')}
\arguments{
  \item{url}{URL of running biouml server}
  \item{user}{BioUML user, empty string for anonymous login}
  \item{pass}{password}
}
\value{
  Invisible connection to BioUML server, connection is a list with following items:
  \item{user}{the name of user used for connection}
  \item{pass}{password in plain text}
  \item{url}{URL of biouml server}
  \item{sessionId}{session id returned by BioUML server}
}
\examples{\dontrun{
## connect to public BioUML server at ie.biouml.org
biouml.login("ie.biouml.org")
getOption("biouml_connection")
}}
