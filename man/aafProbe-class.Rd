\name{aafProbe-class}
\docType{class}
\alias{aafProbe-class}
\title{Class aafProbe, a class for Probe ids}
\description{
  An abstraction for Affymetrix ProbeSet ids.
}

\section{Objects from the Class}{
Objects are generally created by the \code{\link{aafProbe}}
constructor.  Objects can also be created manually by calls of the
form \code{new("aafProbe", id)}.
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
      (aafProbe): Returns a URL to the annotation found in the
      Affymetrix NetAffx Analysis Center.
    }
  }
  See generic implementations of \code{\link{getText}},
  \code{\link{getHTML}}, and \code{\link{getTD}}.
}

\author{Colin A. Smith, \email{annaffy@colinsmith.org}}

\seealso{\link{aafProbe}}

\keyword{classes}
