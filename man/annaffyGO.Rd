\name{annaffyGO}
\alias{annaffyGO}
\title{Subset of annotation data file for GO in the hgu95av2 package}
\description{
This is an R environment object containing key and value pairs for the mappings between probe identifiers (key) and Gene Ontology identifiers (value) defined by Gene Ontology consortium. Keys can be accessed using ls(name) and values using get(key, name) or multiget(keys, name). Values are named vectors of length 1 or greater depending on whether a given probe identifier can be mapped to only one or more Gene Ontology ids. Names for values are the evidence codes for the GO ids. NA is assigned to probe identifiers that can not be mapped to any Gene Ontology id at this time. Mappings between Gene Ontology ids an Gene Ontology terms and other information are available in a separate data package named GO. Above mappings are obtained using data from LocusLink (www.ncbi.nlm.nih.gov/LocusLink) based on unified mappings between probe identifiers and LocusLink ids.
}
\details{
Source data built:
LocusLink built: May 15, 2003.\url{ftp://ftp.ncbi.nih.gov/refseq/LocusLink/LL\_tmpl.gz}.
Package built: Thu May 15 06:51:08 2003
Contains 250 randomly selected probes
}
\references{
\url{ftp://ftp.ncbi.nih.gov/refseq/LocusLink/LL_tmpl.gz}
}
\examples{
xx <- ls(env = annaffyGO)
get(xx[1], env = annaffyGO)
}
\keyword{datasets}

