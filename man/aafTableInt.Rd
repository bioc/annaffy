\name{aafTableInt}
\alias{aafTableInt}
\title{Constructor for aafTable objects from ExpressionSets}
\description{
  Constructs an \code{aafTable} object containing expression values
  given an ExpressionSet.

  In the resulting HTML table, the expression values will have
  backgrounds with varying intensities of green depending on the
  expression measure.
}

\usage{
aafTableInt(exprSet, colnames = sampleNames(exprSet), 
            probeids = featureNames(exprSet))
}
\arguments{
  \item{exprSet}{object of class \code{ExpressionSet}}
  \item{colnames}{character vector of column names}
  \item{probeids}{character vector of probe ids associated with each row}
}

\value{
  An \code{aafTable} object.
}

\author{Colin A. Smith, \email{annaffy@colinsmith.org}}

\seealso{
  \link{aafTable-class},
  \link{aafIntensity}
}

\keyword{file}
