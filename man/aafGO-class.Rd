\name{aafGO-class}
\docType{class}
\alias{aafGO-class}
\alias{getText,aafGO-method}
\alias{getURL,aafGO-method}
\alias{getHTML,aafGO-method}
\alias{getTD,aafGO-method}
\alias{getCSS,aafGO-method}
\title{Class aafGO, a class for gene ontology ids}
\description{
  An abstraction for gene ontology ids from Bioconductor data
  packages. This class is actually extends \code{aafList} and holds
  \code{aafGOItem} objects which have the actual annotation data.
}

\section{Objects from the Class}{
Objects are generally created by the \code{\link{aafGO}} constructor.
Objects can also be created manually by calls of the form
\code{new("aafGO", list(goitems))}.
}
\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{list}}
  }
}
\section{Extends}{
Class \code{aafList}, from data part.
}

\section{Methods}{
  \describe{
    \item{getText}{
      (aafGO): Returns a comma delimeted list of the individual
      \code{aafGOItem} objects.
    }
    \item{getURL}{
      (aafGO): Returns a single URL to an AmiGO page which displays
      all the gene ontology identifiers in an hierarchical listing.
    }
    \item{getHTML}{
      (aafGO): Returns an HTML representation of each of the
      individual \code{aafGOItem} objects, concatenated together.
    }
    \item{getTD}{
      (aafGO): Returns an HTML table cell representation with the
      class set to "aafGO".
    }
    \item{getCSS}{
      (aafGOItem): Returns a line of CSS that indents GOItem paragraphs.
    }
  }
}

\author{Colin A. Smith, \email{annaffy@colinsmith.org}}

\seealso{
  \link{aafGO},
  \link{aafGOItem},
  \link{aafList}
}

\keyword{classes}
