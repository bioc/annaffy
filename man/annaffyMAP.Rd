\name{annaffyMAP}
\alias{annaffyMAP}
\title{Subset of annotation data file for MAP in the hgu95av2 package}
\description{
This is an R environment (hash table like) object containing key and value pairs for the mappings between probe identifiers (key) and cytoband locations (value) on a chromosome. Keys can be accessed using ls(name of the environment) and values using get(key, name of the environment) or multiget(keys, name of the environment). Values are vectors of length 1. Mappings between probe ids and cytoband locations were obtained through their mappings to LocusLink ids. NA is assigned to probe identifiers that can not be mapped to any cytoband at this time. 
}
\details{
Mappings were based on data provided by LocusLink

Source data built:
LocusLink built: January 13, 2004.\url{ftp://ftp.ncbi.nih.gov/refseq/LocusLink/LL\_tmpl.gz}.
Package built: Wed Jan 14 22:11:05 2004
Contains 250 randomly selected probes
}
\references{
\url{ftp://ftp.ncbi.nih.gov/refseq/LocusLink/LL_tmpl.gz}
}
\examples{
	require("annotate") || stop("annotate unavailable")
	xx <- ls(env = annaffyMAP)
	if(length(xx) > 0){
		# Using get for value of the first key
		get(xx[1], annaffyMAP )
		#Using multiget for a few keys
		if(length(xx) >= 3){
			multiget(xx[1:3], annaffyMAP )
			#Using lookUp of annotate(> 1.3.4)
			lookUp(xx[1:3],"annaffy","MAP")
		}
	}
}
\keyword{datasets}

