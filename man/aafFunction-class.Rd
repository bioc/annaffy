\name{aafFunction-class}
\docType{class}
\alias{aafFunction-class}
\title{Class aafFunction, a class for gene product functions}
\description{
  An abstraction for gene product function from Bioconductor data packages.
}

\section{Objects from the Class}{
Objects are generally created by the \code{\link{aafFunction}}
constructor.  Objects can also be created manually by calls of the
form \code{new("aafFunction", description)}.
}
\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{character}}
  }
}
\section{Extends}{
Class \code{character}, from data part.
}

\section{Methods}{
No methods defined with class "aafFunction" in the signature.
See generic implementations of \code{\link{getText}}, \code{\link{getURL}},
\code{\link{getHTML}}, \code{\link{getTD}}, and \code{\link{getCSS}}.
}

\author{Colin A. Smith, \email{annaffy@colinsmith.org}}

\seealso{\link{aafFunction}}

\keyword{classes}
