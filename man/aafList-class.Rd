\name{aafList-class}
\docType{class}
\alias{aafList-class}
\alias{aafList}
\title{Class aafList, a specialized subclass of list}
\description{
  A class for lists of annotation data objects.
}

\section{Objects from the Class}{
Objects are generally created by any of the annotation data constructors
that are also part of this package. Objects can also be created manually 
by calls of the form \code{new("aafList", list)}.
}
\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{list}}
  }
}
\section{Extends}{
Class \code{list}, from data part.
}

\section{Methods}{
  \describe{
    \item{getText}{
      (aafList): Returns a character vector containing textual
      representations of every item.
    }
    \item{getURL}{
      (aafList): Returns a character vector containing single URLs
      (if possible) of every item.
    }
    \item{getHTML}{
      (aafList): Returns a character vector containing HTML
      representations of every item.
    }
    \item{getTD}{
      (aafList): Returns a character vector containing HTML table
      cell representations of every item.
    }
    \item{getCSS}{
      (aafList): Returns getCSS() of the first item in the list.
    }
    \item{[}{
      (aafList): Returns a subset of aafList as another \code{aafList} 
      object.
    }
  }
}

\author{Colin A. Smith, \email{annaffy@colinsmith.org}}

\keyword{classes}
