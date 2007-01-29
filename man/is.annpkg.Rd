\name{is.annpkg}
\alias{is.annpkg}
\title{Determine if packages contain annotation}
\description{
  Checks to see that the given packages contain all the necessary
  annotation environments to be usable by annaffy.
}

\usage{
  is.annpkg(packages, lib.loc = NULL)
}
\arguments{
  \item{packages}{character vector containing package names to check}
  \item{lib.loc}{
    a character vector with path names of \R libraries, or \code{NULL}.
	The default value of \code{NULL} corresponds to all libraries
	currently known. If the default is used, the loaded packages
	are searched before the libraries.
  }
}
\value{
  A logical vector indicating whether the packages contain annotation data.
}
\author{Colin A. Smith, \email{annaffy@colinsmith.org}}
\examples{
pkgnames <- installed.packages()[,"Package"]
pkgnames <- pkgnames[1:5]
pkgnames[is.annpkg(pkgnames)]
}
\keyword{ file }
