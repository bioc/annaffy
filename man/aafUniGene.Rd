\name{aafUniGene}
\alias{aafUniGene}
\title{Constructor for aafUniGene objects}
\description{
  For the given \code{probeids}, constructs a list of
  \code{aafUniGene} objects containing annotation data from the
  \code{chip} data package.
}

\usage{
aafUniGene(probeids, chip)
}
\arguments{
  \item{probeids}{character vector containing probe ids}
  \item{chip}{name of the chip data package}
}

\value{
  An \code{aafList} of \code{aafUniGene} objects. NA values are returned as
  empty objects.
}

\author{Colin A. Smith, \email{annaffy@colinsmith.org}}

\examples{
probes <- ls(annaffyUNIGENE)
ugs <- aafUniGene(probes, "annaffy")
show(ugs[6:10])
}

\seealso{\link{aafUniGene-class}}

\keyword{ file }