\name{annaffyCHRLOC}
\alias{annaffyCHRLOC}
\title{Subset of annotation data file for CHRLOC in the hgu95av2 package}
\description{
This is an R environment object containing key and value pairs for the mappings between probe identifiers (key) and chromosomal location of genes (value). Keys can be accessed using ls(name) and values using get(key, name) or multiget(keys, name). Values are named vectors of signed integers of length 1 or more depending on whether a given probe id can be mapped to a single or multiple chromosomal locations. + and - signs are used to indicate the the strand of the chromosome. The names give the chromosome number of concern. NA is assigned to probe identifiers that can not be mapped to any chromosomal location data at this time. Mappings are obtained using data from LocusLink (www.ncbi.nlm.nih.gov/LocusLink) and Human Genome Project (http://genome.cse.ucsc.edu/goldenPath) based on unified mappings between probe identifiers and LocusLink ids and LocusLink ids and gene locations. 
}
\details{
Source data built:
Human Genome Project built: 10april2003.\url{http://www.genome.ucsc.edu/goldenPath/10april2003/database/}.
Package built: Thu May 15 06:51:05 2003
Contains 250 randomly selected probes
}
\references{
\url{http://www.genome.ucsc.edu/goldenPath/10april2003/database/}
}
\examples{
xx <- ls(env = annaffyCHRLOC)
get(xx[1], env = annaffyCHRLOC)
}
\keyword{datasets}

