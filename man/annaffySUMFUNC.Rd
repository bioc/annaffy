\name{annaffySUMFUNC}
\alias{annaffySUMFUNC}
\title{Subset of annotation data file for SUMFUNC in the hgu95av2 package}
\description{
This is an R environment object containing key and value pairs for the mappings between probe identifiers (key) and summaries of the function (value), which are brief descriptions of the function of the products of genes. Keys can be accessed using ls(name) and values using get(key, name) or multiget(keys, name). Values are vectors of length 1. NA is assigned to probe identifiers that can not be mapped to any summary of function at this time.  
}
\details{
Mappings were based on data provided by LocusLink

Source data built:
LocusLink built: June 23, 2003.\url{ftp://ftp.ncbi.nih.gov/refseq/LocusLink/LL\_tmpl.gz}.
Package built: Thu Jun 26 05:23:11 2003
}
\references{
\url{ftp://ftp.ncbi.nih.gov/refseq/LocusLink/LL_tmpl.gz}
}
\examples{
xx <- ls(env = annaffySUMFUNC)
get(xx[1], env = annaffySUMFUNC)
}
\keyword{datasets}

