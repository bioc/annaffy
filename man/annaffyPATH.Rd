\name{annaffyPATH}
\alias{annaffyPATH}
\title{Subset of annotation data file for PATH in the hgu95av2 package}
\description{
This is an R environment object containing key and value pairs for the mappings between probe identifiers (key) and pathway identifiers defined by KEGG (value). Keys can be accessed using ls(name) and values using get(key, name) or multiget(keys, name). Values may be vectors of length 1 or greater depending on whether a given probe identifier can be associated with only one or more pathways. NA is assigned to probe identifiers that can not be  associated with any pathway at this time. Mappings between pathway ids to pathway names can be found in a separate data package named KEGG. Above mappings are obtained using data from LocusLink (www.ncbi.nlm.nih.gov/LocusLink) and KEGG (www.genome.ad.jp/kegg).
}
\details{
Source data built:
KEGG built: Release 26.0 (April 2003).\url{ftp://ftp.genome.ad.jp/pub/kegg/pathways}.
Package built: Thu May 15 06:51:11 2003
Contains 250 randomly selected probes
}
\references{
\url{ftp://ftp.genome.ad.jp/pub/kegg/pathways}
}
\examples{
xx <- ls(env = annaffyPATH)
get(xx[1], env = annaffyPATH)
}
\keyword{datasets}

