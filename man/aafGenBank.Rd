\name{aafGenBank}
\alias{aafGenBank}
\title{Constructor for aafGenBank objects}
\description{
  For the given \code{probeids}, constructs an \code{aafList} of
  \code{aafGenBank} objects containing annotation data from the
  \code{chip} data package.
}

\usage{
aafGenBank(probeids, chip)
}
\arguments{
  \item{probeids}{character vector containing probe ids}
  \item{chip}{name of the chip data package}
}

\value{
  An \code{aafList} of \code{aafGenBank} objects. NA values are returned as
  empty objects.
}

\author{Colin A. Smith, \email{annaffy@colinsmith.org}}

\examples{
if (require(hgu95av2)) {
    data(aafExpr)
    probes <- geneNames(aafExpr)
    gbs <- aafGenBank(probes, "hgu95av2")
    show(gbs[6:10])
}
}

\seealso{\link{aafGenBank-class}}

\keyword{ file }
