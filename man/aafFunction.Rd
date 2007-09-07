\name{aafFunction}
\alias{aafFunction}
\title{Constructor for aafFunction objects}
\description{
  For the given \code{probeids}, constructs an \code{aafList} of
  \code{aafFunction} objects containing annotation data from the
  \code{chip} data package.
}

\usage{
aafFunction(probeids, chip)
}
\arguments{
  \item{probeids}{character vector containing probe ids}
  \item{chip}{name of the chip data package}
}

\value{
  An \code{aafList} of \code{aafFunction} objects. NA values are
  returned as empty objects.
}

\author{Colin A. Smith, \email{annaffy@colinsmith.org}}

\examples{
if (require(hgu95av2)) {
    data(aafExpr)
    probes <- featureNames(aafExpr)
    functions <- aafFunction(probes, "hgu95av2.db")
    show(functions[6:10])
}
}

\seealso{\link{aafFunction-class}}

\keyword{ file }
