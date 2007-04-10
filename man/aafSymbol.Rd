\name{aafSymbol}
\alias{aafSymbol}
\title{Constructor for aafSymbol objects}
\description{
  For the given \code{probeids}, constructs a list of
  \code{aafSymbol} objects containing annotation data from the
  \code{chip} data package.
}

\usage{
aafSymbol(probeids, chip)
}
\arguments{
  \item{probeids}{character vector containing probe ids}
  \item{chip}{name of the chip data package}
}

\value{
  An \code{aafList} of \code{aafSymbol} objects. NA values are returned as
  empty objects.
}

\author{Colin A. Smith, \email{annaffy@colinsmith.org}}

\examples{
if (require(hgu95av2)) {
    data(aafExpr)
    probes <- featureNames(aafExpr)
    symbols <- aafSymbol(probes, "hgu95av2")
    show(symbols[6:10])
}
}

\seealso{\link{aafSymbol-class}}

\keyword{ file }
