\name{aafCytoband}
\alias{aafCytoband}
\title{Constructor for aafCytoband objects}
\description{
  For the given \code{probeids}, constructs an \code{aafList} of
  \code{aafCytoband} objects containing annotation data from the
  \code{chip} data package.
}

\usage{
aafCytoband(probeids, chip)
}
\arguments{
  \item{probeids}{character vector containing probe ids}
  \item{chip}{name of the chip data package}
}

\value{
  An \code{aafList} of \code{aafCytoband} objects. NA values are returned as
  empty objects.
}

\author{Colin A. Smith, \email{annaffy@colinsmith.org}}

\examples{
probes <- ls(annaffyMAP)
bands <- aafCytoband(probes, "annaffy")
show(bands[6:10])
}

\seealso{\link{aafCytoband-class}}

\keyword{ file }
