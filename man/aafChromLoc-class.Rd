\name{aafChromLoc-class}
\docType{class}
\alias{aafChromLoc-class}
\title{Class aafChromLoc, a class for gene chromosome locations}
\description{
  An abstraction for gene chromosome locations from Bioconductor
  data packages.
}

\section{Objects from the Class}{
Objects are generally created by the \code{\link{aafChromLoc}}
constructor.  Objects can also be created manually by calls of the
form \code{new("aafChromLoc", description)}.
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
No methods defined with class "aafChromLoc" in the signature.
See generic implementations of \code{\link{getText}}, \code{\link{getURL}},
\code{\link{getHTML}}, \code{\link{getTD}}, and \code{\link{getCSS}}.
}

\author{Colin A. Smith, \email{annaffy@colinsmith.org}}

\seealso{\link{aafChromLoc}}

\keyword{classes}
