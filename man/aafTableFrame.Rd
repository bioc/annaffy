\name{aafTableFrame}
\alias{aafTableFrame}
\title{Constructor for aafTable objects from data frames}
\description{
  Constructs an \code{aafTable} object given a data frame.
}

\usage{
aafTableFrame(frame, colnames = names(df), probeids = row.names(frame), 
              signed = FALSE)
}
\arguments{
  \item{frame}{data frame to be converted to the table}
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