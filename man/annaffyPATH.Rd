\name{annaffyPATH}
\alias{annaffyPATH}
\title{Subset of annotation data file for PATH in the hgu95av2 package}
\description{
This is an R environment (hash table like) object containing key and value pairs for the mappings between probe identifiers (key) and pathway identifiers defined by KEGG (value). Keys can be accessed using ls(name of the environment) and values using get(key, name of the environment) or multiget(keys, name of the environment). Values may be vectors of length 1 or greater depending on whether a given probe identifier can be associated with only one or more pathways. Mappings between probe ids and pathway ids were obtained their mappings to LocusLink ids. NA is assigned to probe identifiers that can not be  associated with any pathway at this time. Mappings between pathway ids to pathway names can be found in a separate data package named KEGG. 
}
\details{
Mappings were based on data provided by KEGG

Source data built:
KEGG built: Release 29.0 (January 2004).\url{ftp://ftp.genome.ad.jp/pub/kegg/pathways}.
Package built: Wed Jan 14 22:11:08 2004
Contains 250 randomly selected probes
}
\references{
\url{ftp://ftp.genome.ad.jp/pub/kegg/pathways}
}
\examples{
	require("annotate") || stop("annotate unavailable")
	xx <- ls(env = annaffyPATH)
	if(length(xx) > 0){
		# Using get for value of the first key
		get(xx[1], annaffyPATH )
		#Using multiget for a few keys
		if(length(xx) >= 3){
			multiget(xx[1:3], annaffyPATH )
			#Using lookUp of annotate(> 1.3.4)
			lookUp(xx[1:3],"annaffy","PATH")
		}
	}
}
\keyword{datasets}

