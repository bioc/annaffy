\name{aafPubMed-class}
\docType{class}
\alias{aafPubMed-class}
\alias{getURL,aafPubMed-method}
\alias{getHTML,aafPubMed-method}
\alias{getTD,aafPubMed-method}
\alias{getCSS,aafPubMed-method}
\title{Class aafPubMed, a class for PubMed ids}
\description{
  An abstraction for LocusLink ids from Bioconductor data packages.
}

\section{Objects from the Class}{
Objects are generally created by the \code{\link{aafPubMed}}
constructor.  Objects can also be created manually by calls of the
form \code{new("aafPubMed", id)}.
}
\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{integer}}
  }
}
\section{Extends}{
Class \code{integer}, from data part.
}

\section{Methods}{
  \describe{
    \item{getURL}{
      (aafPubMed): Returns a single URL to the corresponding
      abstracts in NCBI's PubMed database.
    }
    \item{getHTML}{
      (aafPubMed): Returns an HTML link along with the number of
      abstracts.
    }
    \item{getTD}{
      (aafPubMed): Returns an HTML table cell representation with the
      class set to "aafPubMed".
    }
    \item{getCSS}{
      (aafPubMed): Returns a line of CSS which centers the PubMed
      link.
    }
  }
}

\author{Colin A. Smith, \email{annaffy@colinsmith.org}}

\seealso{\link{aafPubMed}}

\keyword{classes}
