\name{annaffyACCNUM}
\alias{annaffyACCNUM}
\title{Subset of annotation data file for ACCNUM in the hgu95av2 package}
\description{
This is an R environment (hash table like) object containing key and value pairs for the mappings between probe identifiers (key) and GenBank accession numbers (value). Keys can be accessed using ls(name of the environment) and values using get(key, name of the environment) or multiget (keys, name of the environment). Values are vectors of length 1. NA is assigned to probe identifiers that can not be mapped to a GenBank accession number. Mappings between probe ids and GenBank accession numbers are assumed to be provided by users. 
}
\details{
Mappings were based on data provided by LocusLink

Source data built:
LocusLink built: January 13, 2004.\url{ftp://ftp.ncbi.nih.gov/refseq/LocusLink/LL\_tmpl.gz}.
Package built: Wed Jan 14 22:11:00 2004
Contains 250 randomly selected probes
}
\references{
\url{ftp://ftp.ncbi.nih.gov/refseq/LocusLink/LL_tmpl.gz}
}
\examples{
	require("annotate") || stop("annotate unavailable")
	xx <- ls(env = annaffyACCNUM)
	if(length(xx) > 0){
		# Using get for value of the first key
		get(xx[1], annaffyACCNUM )
		#Using multiget for a few keys
		if(length(xx) >= 3){
			multiget(xx[1:3], annaffyACCNUM )
			#Using lookUp of annotate(> 1.3.4)
			lookUp(xx[1:3],"annaffy","ACCNUM")
		}
	}
}
\keyword{datasets}

