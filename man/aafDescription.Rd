\name{aafDescription}
\alias{aafDescription}
\title{Constructor for aafDescription objects}
\description{
  For the given \code{probeids}, constructs an \code{aafList} of
  \code{aafDescription} objects containing annotation data from the
  \code{chip} data package.
}

\usage{
aafDescription(probeids, chip)
}
\arguments{
  \item{probeids}{character vector containing probe ids}
  \item{chip}{name of the chip data package}
}

\value{
  An \code{aafList} of \code{aafDescription} objects. NA values are returned as
  empty objects.
}

\author{Colin A. Smith, \email{annaffy@colinsmith.org}}

\examples{
probes <- ls(annaffyGENENAME)
descriptions <- aafDescription(probes, "annaffy")
show(descriptions[6:10])
}

\seealso{\link{aafDescription-class}}

\keyword{ file }