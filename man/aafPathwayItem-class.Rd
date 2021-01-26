\name{aafPathwayItem-class}
\docType{class}
\alias{aafPathwayItem-class}
\alias{aafPathwayItem}
\alias{getText,aafPathwayItem-method}
\alias{getURL,aafPathwayItem-method}
\alias{getHTML,aafPathwayItem-method}
\alias{show,aafPathwayItem-method}
\title{Class aafPathwayItem, a class for KEGG pathway id elements}
\description{
  An abstraction for KEGG pathway id elements from Bioconductor
  data packages. Multiple instances of this class are held by the
  wrapper class \code{aafPathway}.
}

\section{Objects from the Class}{
Objects are generally created by the \code{\link{aafPathway}} constructor.
Objects can also be created manually by calls of the form
\code{new("aafPathwayItem", id, name, enzyme)}.
}
\section{Slots}{
  \describe{
    \item{\code{id}:}{
      Object of class \code{character} containing KEGG pathway id
    }
    \item{\code{name}:}{
      Object of class \code{character} containing textual name (no longer supported)
    }
    \item{\code{enzyme}:}{
      Object of class \code{character} containing the Enzyme
      Commision number if applicable
    }
  }
}

\section{Methods}{
  \describe{
    \item{getText}{
      (aafPathwayItem): Returns textual representation formatted
      "\code{id}: \code{name}".
    }
    \item{getURL}{
      (aafPathwayItem): Returns a URL to the corresponding entry
      in the Kyoto Encyclopedia of Genes and Genomes database. If
      there is a corresponding EC number, it will be highlighted
      in red.
    }
    \item{getHTML}{
      (aafPathwayItem): Returns an HTML representation including the
      URL link and pathway name.
    }
  }
}

\author{Colin A. Smith, \email{annaffy@colinsmith.org}}

\seealso{
  \link{aafPathway-class},
  \link{aafPathway}
}

\keyword{classes}
