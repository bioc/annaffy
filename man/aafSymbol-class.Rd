\name{aafSymbol-class}
\docType{class}
\alias{aafSymbol-class}
\title{Class aafSymbol, a class for gene symbols}
\description{
  An abstraction for gene symbol from Bioconductor data packages.
}

\section{Objects from the Class}{
Objects are generally created by the \code{\link{aafSymbol}}
constructor.  Objects can also be created manually by calls of the
form \code{new("aafSymbol", description)}.
}
\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{character} with gene symbol}
  }
}
\section{Extends}{
Class \code{character}, from data part.
}

\section{Methods}{
No methods defined with class "aafSymbol" in the signature.
See generic implementations of \code{\link{getText}}, \code{\link{getURL}},
\code{\link{getHTML}}, and \code{\link{getTD}}.
}

\author{Colin A. Smith, \email{annaffy@colinsmith.org}}

\seealso{\link{aafSymbol}}

\keyword{classes}
