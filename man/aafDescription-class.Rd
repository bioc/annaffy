\name{aafDescription-class}
\docType{class}
\alias{aafDescription-class}
\title{Class aafDescription, a class for gene descriptions}
\description{
  An abstraction for gene description from Bioconductor data packages.
}

\section{Objects from the Class}{
Objects are generally created by the \code{\link{aafDescription}}
constructor.  Objects can also be created manually by calls of the
form \code{new("aafDescription", description)}.
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
No methods defined with class "aafDescription" in the signature.
See generic implementations of \code{\link{getText}}, \code{\link{getURL}},
\code{\link{getHTML}}, and \code{\link{getTD}}.
}

\author{Colin A. Smith, \email{annaffy@colinsmith.org}}

\seealso{\link{aafDescription}}

\keyword{classes}
