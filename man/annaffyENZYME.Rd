\name{annaffyENZYME}
\alias{annaffyENZYME}
\title{Subset of annotation data file for ENZYME in the hgu95av2 package}
\description{
This is an R environment object containing key and value pairs for the mappings between probe identifiers (key) and Enzyme Commission numbers (value) with the leading EC: portion omitted. Keys can be accessed using ls(name) and values using get(key, name)
}
\details{
Source data built:
KEGG built: Release 26.0 (April 2003).\url{ftp://ftp.genome.ad.jp/pub/kegg/pathways}.
Package built: Thu May 15 06:51:08 2003
Contains 250 randomly selected probes
}
\references{
\url{ftp://ftp.genome.ad.jp/pub/kegg/pathways}
}
\examples{
xx <- ls(env = annaffyENZYME)
get(xx[1], env = annaffyENZYME)
}
\keyword{datasets}

