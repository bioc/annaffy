\name{aaf.handler}
\alias{aaf.handler}
\title{Handle feching annotation data columns}
\description{
  Dispatches requests for annotation data to the correct function. Alternatively
  returns a list of all the columns it supports.
}

\usage{
  aaf.handler(probeids, chip, name)
}
\arguments{
  \item{probeids}{character vector containing probe ids}
  \item{chip}{name of chip}
  \item{name}{name of the column of data to return}
}
\value{
  An \code{aafList} containing objects of the propper class.
  
  If no arguments are passed, it will return a character vector of the columns
  currently supported.
}
\note{
  Written at the NASA Center for Computational Astrobiology \cr
  \url{http://cca.arc.nasa.gov/}
}
\author{Colin A. Smith, \email{annaffy@colinsmith.org}}
\keyword{ file }
