\name{aafChromosome}
\alias{aafChromosome}
\title{Constructor for aafChromosome objects}
\description{
  For the given \code{probeids}, constructs an \code{aafList} of
  \code{aafChromosome} objects containing annotation data from the
  \code{chip} data package.
}

\usage{
aafChromosome(probeids, chip)
}
\arguments{
  \item{probeids}{character vector containing probe ids}
  \item{chip}{name of the chip data package}
}

\value{
  An \code{aafList} of \code{aafChromosome} objects. NA values are
  returned as empty objects.
}

\author{Colin A. Smith, \email{annaffy@colinsmith.org}}

\examples{
probes <- ls(annaffyCHR)
chromosomes <- aafChromosome(probes, "annaffy")
show(chromosomes[6:10])
}

\seealso{\link{aafChromosome-class}}

\keyword{ file }