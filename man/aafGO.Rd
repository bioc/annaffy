\name{aafGO}
\alias{aafGO}
\title{Constructor for aafGO objects}
\description{
  For the given \code{probeids}, constructs an \code{aafList} of
  \code{aafGO} objects containing annotation data from the
  \code{chip} data package.
}

\usage{
aafGO(probeids, chip)
}
\arguments{
  \item{probeids}{character vector containing probe ids}
  \item{chip}{name of the chip data package}
}

\value{
  An \code{aafList} of \code{aafGO} objects. NA values are returned as
  empty objects.
}

\author{Colin A. Smith, \email{annaffy@colinsmith.org}}

\examples{
probes <- ls(annaffyGO)
gos <- aafGO(probes, "annaffy")
show(gos[6:10])
}

\seealso{\link{aafGO-class}}

\keyword{ file }
