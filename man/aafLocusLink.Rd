\name{aafLocusLink}
\alias{aafLocusLink}
\title{Constructor for aafLocusLink objects}
\description{
  For the given \code{probeids}, constructs an \code{aafList} of
  \code{aafLocusLink} objects containing annotation data from the
  \code{chip} data package.
}

\usage{
aafLocusLink(probeids, chip)
}
\arguments{
  \item{probeids}{character vector containing probe ids}
  \item{chip}{name of the chip data package}
}

\value{
  An \code{aafList} of \code{aafLocusLink} objects. NA values are returned as
  empty objects.
}

\author{Colin A. Smith, \email{annaffy@colinsmith.org}}

\examples{
if (require(hgu95av2)) {
    data(aafExpr)
    probes <- geneNames(aafExpr)
    lls <- aafLocusLink(probes, "hgu95av2")
    show(lls[6:10])
}
}

\seealso{\link{aafLocusLink-class}}

\keyword{ file }
