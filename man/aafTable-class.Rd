\name{aafTable-class}
\docType{class}
\alias{aafTable-class}
\alias{probeids}
\alias{probeids,aafTable-method}
\alias{probeids<-}
\alias{probeids<-,aafTable-method}
\alias{colnames}
\alias{colnames,aafTable-method}
\alias{colnames<-}
\alias{colnames<-,aafTable-method}
\alias{dim.aafTable}
\alias{merge.aafTable}
\alias{rbind.aafTable}
\alias{[,aafTable-method}
\alias{[[,aafTable-method}
\alias{$.aafTable}
\alias{saveHTML}
\alias{saveHTML,aafTable-method}
\alias{saveText}
\alias{saveText,aafTable-method}
\title{Class aafTable, a tabular microarray data class}
\description{
  A class for storing and flexible output of microarray data to
  HTML and text formats.
}

\section{Objects from the Class}{
Objects are generally created by any of the annotation table constructors
that are also part of this package. Objects can also be created manually 
by calls of the form \code{new("aafList", probeids, table)}.
}
\section{Slots}{
  \describe{
    \item{\code{probeids}:}{
      Object of class \code{character} containing the probe ids
      for each row of the table.
    }
    \item{\code{table}:}{
      Object of class \code{list} containing \code{aafList} objects
      all of the same length, representing the columns of the table.
      Each item in the list must have a unique name.
    }
  }
}

\section{Methods}{
  \describe{
    \item{probeids}{
      (aafTable): Returns a character vector containing the probe
      ids for each row of the table.
    }
    \item{probeids<-}{
      (aafTable): Sets the probe ids for the table rows. Can be
      set to \code{character(0)} if unknown or not applicable.
    }
    \item{colnames}{
      (aafTable): Returns a character vector containing the names
      of the columns stored in the table.
    }
    \item{colnames<-}{
      (aafTable): Set the column names for the table. Each must be
      unique.
    }
    \item{dim}{
      (aafTable): Returns the dimensions of the table.
    }
    \item{merge}{
      (aafTable, aafTable, all = FALSE, all.x = all, all.y = all, 
      suffixes = c(".x",".y")): Merges two
      tables together, aligning common probe ids if possible.
      Duplicate column names are given suffixes to make them unique.
      Returns the merged table.
    }
    \item{rbind}{
      (aafTable, aafTable, ...): Vertically combines tables by row.
      Requires that column names be identical and that all tables
      either have probe ids defined or not.
    }
    \item{[}{
      Returns a subset of the table based on [row, column]. Indices
      may be passed as integers or probe ids/column names.
    }
    \item{[[}{
      Returns the given table column. This also supports recursive
      subsetting to address columns, then cells, then sub-cells
      (if applicable). See \link{Extract} for more information.
    }
    \item{\$}{
      Returns the given table column.
    }
    \item{saveHTML}{
      (aafTable, filename, title = "Bioconductor Affymetrix Probe
      Listing", colnames = colnames(aafTable), range = 1:dim(aafTable)[1],
      open = FALSE, widget = FALSE):  Saves the table to HTML with
      the specified filename and title. Both the columns and the
      range of table rows can be specified. Range can either be
      specified as a character vector of probe ids or an integer
      vector of row positions. One can also specify whether to open
      the resulting file in the browser and whether to use a widget
      for column selection.
    }
    \item{saveText}{
      (aafTable, filename, header = TRUE, colnames = colnames(aafTable),
      range = 1:dim(aafTable)[1], widget = FALSE): Saves the table
      to tab delimited text with specified filename and optional
      header.  Both the columns and the range of table rows can be
      specified.  Range can either be specified as a character
      vector of probe ids or an integer vector of row positions.
      One can also specify whether to use a widget for column
      selection.
    }
  }
}

\author{Colin A. Smith, \email{annaffy@colinsmith.org}}

\seealso{
  \link{aafTable},
  \link{aafTableFrame},
  \link{aafTableAnn},
  \link{aafTableInt}
}

\keyword{classes}
