\name{aafGenBank-class}
\docType{class}
\alias{aafGenBank-class}
\title{Class aafGenBank, a class for GenBank accession numbers}
\description{
  An abstraction for GenBank accession numbers from Bioconductor data packages.
}

\section{Objects from the Class}{
Objects are generally created by the \code{\link{aafGenBank}}
constructor.  Objects can also be created manually by calls of the
form \code{new("aafGenBank", accnum)}.
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
  \describe{
    \item{getURL}{
      (aafGenBank): Returns a URL to the corresponding entry in NCBI's
      GenBank database.
    }
  }
  See generic implementations of \code{\link{getText}},
  \code{\link{getHTML}}, and \code{\link{getTD}}.
}

\author{Colin A. Smith, \email{annaffy@colinsmith.org}}

\seealso{\link{aafGenBank}}

\keyword{classes}
