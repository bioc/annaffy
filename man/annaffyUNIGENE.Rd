\name{annaffyUNIGENE}
\alias{annaffyUNIGENE}
\title{Subset of annotation data file for UNIGENE in the hgu95av2 package}
\description{
This is an R environment object containing key and value pairs for the mappings between probe identifiers (key) and UniGene cluster ids (value). Keys can be accessed using ls(name) and values using get(key, name) or multiget(keys, name) . Values may be vectors of length 1 or greater depending on whether a given probe identifier can be mapped to only one or more UniGene cluster ids. NA is assigned to probe identifiers that can not be mapped to any UniGene cluster id at this time. Mappings are obtained using data from LocusLink (www.ncbi.nlm.nih.gov/LocusLink) based on unified mappings between probe identifiers and LocusLink ids.
}
\details{
Source data built:
LocusLink built: May 15, 2003.\url{ftp://ftp.ncbi.nih.gov/refseq/LocusLink/LL\_tmpl.gz}.
Package built: Thu May 15 06:51:12 2003
Contains 250 randomly selected probes
}
\references{
\url{ftp://ftp.ncbi.nih.gov/refseq/LocusLink/LL_tmpl.gz}
}
\examples{
xx <- ls(env = annaffyUNIGENE)
get(xx[1], env = annaffyUNIGENE)
}
\keyword{datasets}

