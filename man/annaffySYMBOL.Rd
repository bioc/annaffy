\name{annaffySYMBOL}
\alias{annaffySYMBOL}
\title{Subset of annotation data file for SYMBOL in the hgu95av2 package}
\description{
This is an R environment object containing key and value pairs for the mappings between probe identifiers (key) and gene symbols (value), which are symbols used to report genes. Keys can be accessed using ls(name) and values using get(key, name) or multiget(keys, name). Values are vectors of length 1. NA is assigned to probe identifiers that can not be mapped to any gene symbol at this time. Mappings are obtained using data from LocusLink (www.ncbi.nlm.nih.gov/LocusLink) based on unified mappings between probe identifiers and LocusLink ids.
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
xx <- ls(env = annaffySYMBOL)
get(xx[1], env = annaffySYMBOL)
}
\keyword{datasets}

