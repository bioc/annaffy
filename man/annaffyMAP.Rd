\name{annaffyMAP}
\alias{annaffyMAP}
\title{Subset of annotation data file for MAP in the hgu95av2 package}
\description{
This is an R environment object containing key and value pairs for the mappings between probe identifiers (key) and cytoband locations (value). Keys can be accessed using ls(name) and values using get(key, name) or multiget(keys, name). Values are vectors of length 1. NA is assigned to probe identifiers that can not be mapped to any cytoband at this time. Mappings are obtained using data from LocusLink (www.ncbi.nlm.nih.gov/LocusLink) based on unified mappings between probe identifiers and LocusLink ids.
}
\details{
Source data built:
LocusLink built: May 15, 2003.\url{ftp://ftp.ncbi.nih.gov/refseq/LocusLink/LL\_tmpl.gz}.
Package built: Thu May 15 06:51:09 2003
Contains 250 randomly selected probes
}
\references{
\url{ftp://ftp.ncbi.nih.gov/refseq/LocusLink/LL_tmpl.gz}
}
\examples{
xx <- ls(env = annaffyMAP)
get(xx[1], env = annaffyMAP)
}
\keyword{datasets}

