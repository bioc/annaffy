\name{.aaf.goterm}
\alias{.aaf.goterm}
\title{Fetch Gene Ontology term information}
\description{
  Given a Gene Ontology number, return its type and name.
}

\usage{
.aaf.goterm(num)
}
\arguments{
  \item{num}{Gene Ontology number - should be formatted \code{GO:XXXXXXX}}
}

\value{
  A list with components
  \item{type}{Type of GO record, either Biological Process, Cellular Component,
    or Molecular Function.}
  \item{name}{A character vector containing the GO name.}
}
\note{
  Written at the NASA Center for Computational Astrobiology \cr
  \url{http://cca.arc.nasa.gov/}
}
\author{Colin A. Smith, \email{annaffy@colinsmith.org}}
\keyword{ file }
