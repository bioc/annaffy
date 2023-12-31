\name{aafPathway}
\alias{aafPathway}
\title{Constructor for aafPathway objects}
\description{
  For the given \code{probeids}, constructs an \code{aafList} of
  \code{aafPathway} objects containing annotation data from the
  \code{chip} data package.
}

\usage{
aafPathway(probeids, chip)
}
\arguments{
  \item{probeids}{character vector containing probe ids}
  \item{chip}{name of the chip data package}
}

\value{
  An \code{aafList} of \code{aafPathway} objects. NA values are returned as
  empty objects.
}

\author{Colin A. Smith, \email{annaffy@colinsmith.org}}

\examples{
if (require(hgu95av2.db)) {
    data(aafExpr)
    probes <- featureNames(aafExpr)
    pathways <- aafPathway(probes, "hgu95av2.db")
    show(pathways[6:10])
}
}

\seealso{\link{aafPathway-class}}

\keyword{ file }
