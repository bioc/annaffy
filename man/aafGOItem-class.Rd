\name{aafGOItem-class}
\docType{class}
\alias{aafGOItem-class}
\alias{aafGOItem}
\alias{getText,aafGOItem-method}
\alias{getURL,aafGOItem-method}
\alias{getHTML,aafGOItem-method}
\alias{show,aafGOItem-method}
\title{Class aafGOItem, a class for gene ontology id elements}
\description{
  An abstraction for gene ontology id elements from Bioconductor
  data packages. Multiple instances of this class are held by the
  wrapper class \code{aafGO}.
}

\section{Objects from the Class}{
Objects are generally created by the \code{\link{aafGO}} constructor.
Objects can also be created manually by calls of the form
\code{new("aafGOItem", id, name, type)}.
}
\section{Slots}{
  \describe{
    \item{\code{id}:}{
      Object of class \code{character} containing GO id
    }
    \item{\code{name}:}{
      Object of class \code{character} containing textual name
    }
    \item{\code{type}:}{
      Object of class \code{character} containing GO subtype
    }
    \item{\code{evid}:}{
      Object of class \code{character} containing GO evidence code
    }
  }
}
\section{Methods}{
  \describe{
    \item{getText}{
      (aafGOItem): Returns textual representation formatted
      "\code{id}: \code{name}".
    }
    \item{getURL}{
      (aafGOItem): Returns a URL to the corresponding gene ontology
      entry on AmiGO.
    }
    \item{getHTML}{
      (aafGOItem): Returns an HTML representation including the
      URL link, gene ontology name, and rollover subtype.
    }
  }
}

\author{Colin A. Smith, \email{annaffy@colinsmith.org}}

\seealso{
  \link{aafGO-class},
  \link{aafGO}
}

\keyword{classes}
