\name{annaffyACCNUM}
\alias{annaffyACCNUM}
\title{Subset of annotation data file for ACCNUM in the hgu95av2 package}
\description{
This is an R environment object containing key and value pairs for the mappings between probe identifiers (key) and GenBank accession numbers (value). Keys can be accessed using ls(name) and values using get(key, name) or multiget (keys, name). Values are vectors of length 1. NA is assigned to probe identifiers that can not be mapped to a GenBank accession number. Mappings between probe ids and GenBank accession numbers are assumed to be provided by users. 
}
\details{
Source data built:
LocusLink built: May 15, 2003.\url{ftp://ftp.ncbi.nih.gov/refseq/LocusLink/LL\_tmpl.gz}.
Package built: Thu May 15 06:51:05 2003
Contains 250 randomly selected probes
}
\references{
\url{ftp://ftp.ncbi.nih.gov/refseq/LocusLink/LL_tmpl.gz}
}
\examples{
xx <- ls(env = annaffyACCNUM)
get(xx[1], env = annaffyACCNUM)
}
\keyword{datasets}

