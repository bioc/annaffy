\name{aafChromLoc}
\alias{aafChromLoc}
\title{Constructor for aafChromLoc objects}
\description{
  For the given \code{probeids}, constructs an \code{aafList} of
  \code{aafChromLoc} objects containing annotation data from the
  \code{chip} data package.
}

\usage{
aafChromLoc(probeids, chip)
}
\arguments{
  \item{probeids}{character vector containing probe ids}
  \item{chip}{name of the chip data package}
}

\value{
  An \code{aafList} of \code{aafChromLoc} objects. NA values are
  returned as empty objects.
}

\author{Colin A. Smith, \email{annaffy@colinsmith.org}}

\examples{
if (require(hgu95av2.db)) {
    data(aafExpr)
    probes <- featureNames(aafExpr)
    locations <- aafChromLoc(probes, "hgu95av2.db")
    show(locations[6:10])
}
}

\seealso{\link{aafChromLoc-class}}

\keyword{ file }
