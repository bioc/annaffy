\name{aafTable}
\alias{aafTable}
\title{Constructor for aafTable objects}
\description{
  Constructs an \code{aafTable} object given vectors, lists, or
  \code{aafList} objects.
}

\usage{
aafTable(..., items = list(...), colnames = names(items), 
         probeids = character(0), signed = FALSE)
}
\arguments{
  \item{...}{named arguments, one for each column}
  \item{items}{alternatively a named list of the items to be put in the table}
  \item{colnames}{character vector of column names}
  \item{probeids}{character vector of probe ids associated with each row}
  \item{signed}{boolean, should each column be colored based on the sign?}
}

\value{
  An \code{aafTable} object.
}

\author{Colin A. Smith, \email{annaffy@colinsmith.org}}

\seealso{\link{aafTable-class}}

\keyword{ file }
