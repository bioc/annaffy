\name{aafTableAnn}
\alias{aafTableAnn}
\title{Constructor for aafTable objects from annotation data}
\description{
  Constructs an \code{aafTable} object given a set of probe ids
  and desired annotation types.
}

\usage{
aafTableAnn(probeids, chip, colnames = aaf.handler(chip = chip), widget = FALSE)
}
\arguments{
  \item{probeids}{character vector of probe ids}
  \item{chip}{name of the data package in which the annotation data is stored}
  \item{colnames}{character vector of annotation types}
  \item{widget}{boolean, use widget to select columns?}
}

\value{
  An \code{aafTable} object.
}

\author{Colin A. Smith, \email{annaffy@colinsmith.org}}

\seealso{\link{aafTable-class}}

\keyword{ file }
