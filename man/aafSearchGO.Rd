\name{aafSearchGO}
\alias{aafSearchGO}
\title{Find probe ids corresponding to GO ids}
\description{
  Searches Gene Ontology ids for corresponding probe ids in a given
  chip, optionally including descendents.
}

\usage{
aafSearchGO(chip, ids, descendents = TRUE, logic = "OR")
}
\arguments{
  \item{chip}{name of the chip data package}
  \item{ids}{numeric or character vector of GO ids}
  \item{descendents}{logical, include GO descendents?}
  \item{logic}{type of logic to use, "AND" or "OR"}
}

\value{
  A character vector of probe ids matching the search criteria.
}

\author{Colin A. Smith, \email{annaffy@colinsmith.org}}

\seealso{\link{aafSearchText}}

\keyword{ file }
