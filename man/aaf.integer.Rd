\name{.aaf.integer}
\alias{.aaf.integer}
\title{Generic constructor for classes extending integer}
\description{
  Retrieve annotation from an integer data source.
}

\usage{
.aaf.integer(probeids, chip, type, class)
}
\arguments{
  \item{probeids}{character vector containing probe ids}
  \item{chip}{name of chip}
  \item{type}{type of annotation}
  \item{class}{class of object to be created}
}

\value{
  A list of objects of class \code{class} containing the annotation 
  data of from the \code{type} dataset for the given \code{probeids}.
  NA values are returned as empty objects.
}
\note{
  Written at the NASA Center for Computational Astrobiology \cr
  \url{http://cca.arc.nasa.gov/}
}
\author{Colin A. Smith, \email{annaffy@colinsmith.org}}
\keyword{ file }
