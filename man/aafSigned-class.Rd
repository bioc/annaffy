\name{aafSigned-class}
\docType{class}
\alias{aafSigned-class}
\alias{aafSigned}
\alias{getTD,aafSigned-method}
\alias{getCSS,aafSigned-method}
\title{Class aafSigned, a class for signed numerical data}
\description{
  A class for displaying signed numerical data with different styles
  depending on the sign.
}

\section{Objects from the Class}{
Objects are generally created by the \code{\link{aafTable}}
constructor.  Objects can also be created manually by calls of the
form \code{new("aafSigned", signedval)}.
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
      (aafSigned): Returns an HTML table cell with class
      differentially set based on sign.  \code{aafSignedPos} is
      used for positive values. \code{aafSignedNeg} is used for
      negative values. \code{aafSignedZero} is used for zero values.
    }
    \item{getCSS}{
      (aafSigned): Returns two lines of CSS that set the cell
      background of positive values light blue and negative values
      light red.
    }
  }
  See generic implementations of \code{\link{getText}},
  \code{\link{getURL}}, and \code{\link{getHTML}}.
}

\author{Colin A. Smith, \email{annaffy@colinsmith.org}}

\seealso{\link{aafTable}}

\keyword{classes}
