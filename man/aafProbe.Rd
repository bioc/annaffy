\name{aafProbe}
\alias{aafProbe}
\title{Constructor for aafProbe objects}
\description{
  For the given \code{probeids}, constructs an \code{aafList} of
  \code{aafProbe} objects.
}

\usage{
aafProbe(probeids)
}
\arguments{
  \item{probeids}{character vector containing probe ids}
}

\value{
  An \code{aafList} of \code{aafProbe} objects.
}

\author{Colin A. Smith, \email{annaffy@colinsmith.org}}

\examples{
probes <- ls(annaffySYMBOL)
probesets <- aafProbe(probes)
getURL(probesets[6:10])
}

\seealso{\link{aafProbe-class}}

\keyword{ file }
