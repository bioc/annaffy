\name{aafSearchText}
\alias{aafSearchText}
\title{Search metadata annotation text}
\description{
  Searches Bioconductor metadata annotation package text for specific
  strings or Perl compatible regular expressions.
}

\usage{
aafSearchText(chip, colnames, text, logic = "OR")
}
\arguments{
  \item{chip}{name of the chip data package}
  \item{colnames}{character vector of metadata column names to search}
  \item{text}{character vector of strings/regular expressons to match}
  \item{logic}{type of logic to use, "AND" or "OR"}
}

\value{
  A character vector of probe ids matching the search criteria.
}

\author{Colin A. Smith, \email{annaffy@colinsmith.org}}

\examples{
aafSearchText("annaffy", "Description", c("kinase", "interferon"))
aafSearchText("annaffy", c("Gene Ontology", "Pathway"), "ribosome")
}

\seealso{\link{aafSearchGO}}

\keyword{ file }