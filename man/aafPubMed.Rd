\name{aafPubMed}
\alias{aafPubMed}
\title{Constructor for aafPubMed objects}
\description{
  For the given \code{probeids}, constructs a list of
  \code{aafPubMed} objects containing annotation data from the
  \code{chip} data package.
}

\usage{
aafPubMed(probeids, chip)
}
\arguments{
  \item{probeids}{character vector containing probe ids}
  \item{chip}{name of the chip data package}
}

\value{
  An \code{aafList} of \code{aafPubMed} objects. NA values are returned as
  empty objects.
}

\author{Colin A. Smith, \email{annaffy@colinsmith.org}}

\examples{
probes <- ls(annaffyPMID)
pmids <- aafPubMed(probes, "annaffy")
show(pmids[6:10])
}

\seealso{\link{aafPubMed-class}}

\keyword{ file }