\name{aafPathway-class}
\docType{class}
\alias{aafPathway-class}
\title{Class aafPathway, a class for KEGG pathway ids}
\description{
  An abstraction for KEGG pathway ids from Bioconductor data
  packages. This class is actually extends \code{aafList} and holds
  \code{aafPathwayItem} objects which have the actual annotation
  data.
}
\section{Objects from the Class}{
Objects are generally created by the \code{\link{aafPathway}} constructor.
Objects can also be created manually by calls of the form
\code{new("aafPathway", list(pathwayitems))}.
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
      \code{aafPathwayItem} objects.
    }
    \item{getURL}{
      (aafGO): Returns zero length character vector because this
      method is not valid for this class.
    }
    \item{getHTML}{
      (aafGO): Returns an HTML representation of each of the
      individual \code{aafPathwayItem} objects, concatenated together.
    }
    \item{getTD}{
      (aafGO): Returns an HTML table cell representation with the
      class set to "aafPathway".
    }
    \item{getCSS}{
      (aafGO): Returns a line of CSS which intends PathwayItem
      paragraphs.
    }
  }
}

\author{Colin A. Smith, \email{annaffy@colinsmith.org}}

\seealso{
  \link{aafPathway},
  \link{aafPathwayItem},
  \link{aafList}
}

\keyword{classes}
