\name{annaffyCHRLOC}
\alias{annaffyCHRLOC}
\title{Subset of annotation data file for CHRLOC in the hgu95av2 package}
\description{
This is an R environment (hash table like) object containing key and value pairs for the mappings between probe identifiers (key) and transcription startting  positions of genes on chromosomes (value). Keys can be accessed using ls(name of the environment) and values using get(key, name of the environment) or multiget(keys, name of the environment). Values are named vectors of length 1 or more depending on whether a given probe id can be mapped to a single or multiple chromosomes. The names give the chromosome number of concern. NA is assigned to probe identifiers that can not be mapped to any chromosomal location data at this time. Starting positions for genes on the antisense strand have a leading "-" sign (e. g. -1234567). The starting positions for both the sense and antisense strand are number of base pairs measured from the p (5' end of the sense strand) to q (3' end of the sense strand) arms.  When a gene can not be placed on a chromosome with confidence, "random" is appended to the end of the name for a chromosomal location value. Mappings were obtained using refLint.txt.gz and refGene.txt.gz file from Golden Path (\url{http://www.genome.ucsc.edu/goldenPath/}) from the latest release.
}
\details{
Mappings were based on data provided by Human Genome Project

Source data built:
Human Genome Project built: hg16.\url{http://www.genome.ucsc.edu/goldenPath/hg16/database/}.
Package built: Wed Jan 14 22:11:01 2004
Contains 250 randomly selected probes
}
\references{
\url{http://www.genome.ucsc.edu/goldenPath/hg16/database/}
}
\examples{
	require("annotate") || stop("annotate unavailable")
	xx <- ls(env = annaffyCHRLOC)
	if(length(xx) > 0){
		# Using get for value of the first key
		get(xx[1], annaffyCHRLOC )
		#Using multiget for a few keys
		if(length(xx) >= 3){
			multiget(xx[1:3], annaffyCHRLOC )
			#Using lookUp of annotate(> 1.3.4)
			lookUp(xx[1:3],"annaffy","CHRLOC")
		}
	}
}
\keyword{datasets}

