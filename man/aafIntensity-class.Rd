\name{aafIntensity-class}
\docType{class}
\alias{aafIntensity-class}
\alias{aafIntensity}
\alias{getTD,aafIntensity-method}
\title{Class aafIntensity, a class for gene expression values}
\description{
  A class for displaying gene expression values with a green
  background of differing intensities.
}

\section{Objects from the Class}{
Objects are generally created by the \code{\link{aafTableInt}}
constructor.  Objects can also be created manually by calls of the
form \code{new("aafIntensity", intensity)}.
}
\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{numeric}}
  }
}
\section{Extends}{
Class \code{numeric}, from data part.
}

\section{Methods}{
  \describe{
    \item{getTD}{
      (aafIntensity): Returns an HTML table cell with background
      varrying from white to green depending on intensity. Scaling
      is controlled by two options, \code{minIntensity} (fully
      white) and \code{maxIntensity} (fully green), usually set by
      writeHTML.
    }
  }
  See generic implementations of \code{\link{getText}},
  \code{\link{getURL}}, \code{\link{getHTML}}, and \code{\link{getCSS}}.
}

\author{Colin A. Smith, \email{annaffy@colinsmith.org}}

\seealso{\link{aafTableInt}}

\keyword{classes}
