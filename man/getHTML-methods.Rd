\name{getHTML-methods}
\docType{methods}
\title{Methods for function getHTML}
\alias{getHTML-methods}
\alias{getHTML}
\alias{getHTML,ANY-method}
\description{
  Methods to get an HTML representation of an object.
}

\section{Methods}{
  \describe{
    \item{object = ANY}{
      Returns text of \code{object} along with URL link if applicable.
      If \code{object} is floating point, it displays a fixed number
      of significant digits as specified by the \code{sigfigs}
      option (default 6).
    }
  }
}

\note{
  For information about other implementations of this method, 
  see documentation of the respective class.  
}

\seealso{
  \link{aafList-class},
  \link{aafPubMed-class},
  \link{aafGO-class},
  \link{aafGOItem-class},
  \link{aafPathway-class},
  \link{aafPathwayItem-class}
}

\keyword{methods}
