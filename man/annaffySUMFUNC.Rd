\name{annaffySUMFUNC}
\alias{annaffySUMFUNC}
\title{Subset of annotation data file for SUMFUNC in the hgu95av2 package}
\description{
This is an R environment (hash table like) object containing key and value pairs for the mappings between probe identifiers (key) and summaries of the function (value), which are brief descriptions of the functions of the products of genes. Keys can be accessed using ls(name of the environment) and values using get(key, name of the environment) or multiget(keys, name of the environment). Values are vectors of length 1. Mappings between probe ids and descriptions of functions were obtained through their mappings to LocusLink ids. NA is assigned to probe identifiers that can not be mapped to any summary of function at this time.  
}
\details{
Mappings were based on data provided by LocusLink

Source data built:
LocusLink built: January 13, 2004.\url{ftp://ftp.ncbi.nih.gov/refseq/LocusLink/LL\_tmpl.gz}.
Package built: Wed Jan 14 22:11:08 2004
Contains 250 randomly selected probes
}
\references{
\url{ftp://ftp.ncbi.nih.gov/refseq/LocusLink/LL_tmpl.gz}
}
\examples{
	require("annotate") || stop("annotate unavailable")
	xx <- ls(env = annaffySUMFUNC)
	if(length(xx) > 0){
		# Using get for value of the first key
		get(xx[1], annaffySUMFUNC )
		#Using multiget for a few keys
		if(length(xx) >= 3){
			multiget(xx[1:3], annaffySUMFUNC )
			#Using lookUp of annotate(> 1.3.4)
			lookUp(xx[1:3],"annaffy","SUMFUNC")
		}
	}
}
\keyword{datasets}

