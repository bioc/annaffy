\name{aafLocusLink-class}
\docType{class}
\alias{aafLocusLink-class}
\title{Class aafLocusLink, a class for LocusLink ids}
\description{
  An abstraction for LocusLink ids from Bioconductor data packages.
}

\section{Objects from the Class}{
Objects are generally created by the \code{\link{aafLocusLink}}
constructor.  Objects can also be created manually by calls of the
form \code{new("aafLocusLink", id)}.
}
\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{integer}}
  }
}
\section{Extends}{
Class \code{integer}, from data part.
}

\section{Methods}{
  \describe{
    \item{getURL}{
      (aafLocusLink): Returns a URL to the corresponding entry in
      NCBI's LocusLink database. On the rare chance that more than
      one id is defined, more than one URL will be returned.
    }
  }
  See generic implementations of \code{\link{getText}},
  \code{\link{getHTML}}, and \code{\link{getTD}}.
}

\author{Colin A. Smith, \email{annaffy@colinsmith.org}}

\seealso{\link{aafLocusLink}}

\keyword{classes}
