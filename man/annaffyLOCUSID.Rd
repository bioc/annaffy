\name{annaffyLOCUSID}
\alias{annaffyLOCUSID}
\title{Subset of annotation data file for LOCUSID in the hgu95av2 package}
\description{
This is an R environment object containing key and value pairs for the mappings between  probe identifiers (key) and LocusLink ids (values). Keys can be accessed using ls(name) and values using get(key, name) multiget(keys, name). Values may be vectors of length 1 or greater depending on whether a given probe identifier can be mapped to only one or more LocusLink ids. NA is assigned to probe identifiers that can not be mapped to a LocusLink id at this time. Mappings between probe identifiers and LocusLink ids are obtained collectively using data from UniGene (\url{www.ncbi.nlm.nih.gov/UniGene}) and LocusLink (\url{www.ncbi.nlm.nih.gov/LocusLink}).
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
xx <- ls(env = annaffyLOCUSID)
get(xx[1], env = annaffyLOCUSID)
}
\keyword{datasets}

