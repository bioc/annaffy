\name{aafCytoband-class}
\docType{class}
\alias{aafCytoband-class}
\alias{getText,aafCytoband-method}
\alias{getURL,aafCytoband-method}
\alias{show,aafCytoband-method}
\title{Class aafCytoband, a class for cytoband data}
\description{
  An abstraction for cytoband data from Bioconductor data packages.
}

\section{Objects from the Class}{
Objects are generally created by the \code{\link{aafCytoband}}
constructor.  Objects can also be created manually by calls of the
form \code{new("aafCytoband", band, genbank)}.
}
\section{Slots}{
  \describe{
    \item{\code{band}:}{
      Object of class \code{character} containing genomic cytoband
    }
    \item{\code{genbank}:}{
      Object of class \code{character} containing containing GenBank
      accession number
    }
  }
}
\section{Methods}{
  \describe{
    \item{getText}{
      (aafCytoband): Returns text of \code{band}.
    }
    \item{getURL}{
      (aafCytoband): Returns a URL corresponding entry in NCBI's
      cytoband map viewer.
    }
  }
  See generic implementations of \code{\link{getHTML}},
  \code{\link{getTD}}, and \code{\link{getCSS}}.
}

\author{Colin A. Smith, \email{annaffy@colinsmith.org}}

\seealso{
  \link{aafCytoband}
}

\keyword{classes}
