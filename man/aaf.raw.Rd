\name{.aaf.raw}
\alias{.aaf.raw}
\title{Fetch raw annotation data}
\description{
  Retrieve annotation data from a data package, loading the library if 
  necessary.
}

\usage{
.aaf.raw(probeids, chip, type)
}
\arguments{
  \item{probeids}{character vector containing probe ids}
  \item{chip}{name of chip, see details}
  \item{type}{type of annotation, see details}
}
\details{
  The core workings of this function depend on an (informal) protocol
  used in creating the BioConductor Affymetrix annotation data
  packages. Based on currently published (and unpublished) data packages,
  the current protocol includes the following features:

  The package is named after the chip, \code{<chip name>} \cr
  The package contains datasets named \code{<chip name><data type>}
}

\value{
  A list of annotation data for the given probeids. Each list contains a
  sub-list containing the actual data.
}
\note{
  Written at the NASA Center for Computational Astrobiology \cr
  \url{http://cca.arc.nasa.gov/}
}
\author{Colin A. Smith, \email{annaffy@colinsmith.org}}
\keyword{ file }
\keyword{internal}
