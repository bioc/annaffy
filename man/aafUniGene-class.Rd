\name{aafUniGene-class}
\docType{class}
\alias{aafUniGene-class}
\title{Class aafUniGene, a class for UniGene cluster ids}
\description{
  An abstraction for UniGene cluster ids from Bioconductor data packages.
}

\section{Objects from the Class}{
Objects are generally created by the \code{\link{aafUniGene}}
constructor.  Objects can also be created manually by calls of the
form \code{new("aafUniGene", id)}.
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
      (aafUniGene): Returns a URL to the corresponding entry in
      NCBI's UniGene database. On the rare chance that more than
      one id is defined, more than one URL will be returned.
    }
  }
}

\author{Colin A. Smith, \email{annaffy@colinsmith.org}}

\seealso{\link{aafUniGene}}

\keyword{classes}
