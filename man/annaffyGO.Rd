\name{annaffyGO}
\alias{annaffyGO}
\title{Subset of annotation data file for GO in the hgu95av2 package}
\description{
This is an R environment (hash table lilke) object containing key and value pairs for the mappings between probe identifiers (key) and Gene Ontology information collected from Gene Ontology consortium. Keys can be accessed using ls(name of the environment) and values using get(key, name of the environment) or mget(keys, name of the environment). Each probe id is mapped to a list of one (mapped to none or one GO id) or more (mapped to more than one GO ids) elements. When a probe id is mapped to at lest one GO id, each element of the list contains a sub list of three elements named "GOID", "Ontology", and "Evidence". Values for element "GOID" gives the Gene Ontology identifiers the key probe ids corresponding to. Values for element "Ontology" can be an abbreviation of MF (mocular function), BP (biological process), or CC (cellular component) for the GO category the GO id belongs to. Values for element "Evidence" contains an evidence code indicating what kind of evidence is found to support the association of the GO id to the key probe id. The evidence codes in use include:  

\item{IMP}{inferred from mutant phenotype}  
\item{IGI}{inferred from genetic interaction}
\item{IPI}{inferred from physical interaction}  
\item{ISS}{inferred from sequence similarity} 
\item{IDA}{inferred from direct assay}  
\item{IEP}{inferred from expression pattern}  
\item{IEA}{inferred from electronic annotation}  
\item{TAS}{traceable author statement}  
\item{NAS}{non-traceable author statement}  
\item{ND}{no biological data available}  
\item{IC}{inferred by curator}  

Mappings between probe ids and GO ids were obtained through their mappings to LocusLink ids. NA is assigned to probe identifiers that can not be mapped to any Gene Ontology id at this time. Mappings between Gene Ontology ids an Gene Ontology terms and other information are available in a separate data package named GO. 
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
	xx <- ls(env = annaffyGO)
	if(length(xx) > 0){
		# Using get for value of the first key
		get(xx[1], annaffyGO )
		#Using mget for a few keys
		if(length(xx) >= 3){
			mget(xx[1:3], annaffyGO, ifnotfound=NA )
			#Using lookUp of annotate(> 1.3.4)
			lookUp(xx[1:3],"annaffy","GO")
		}
                # Get GO ids. annotate >= 1.3.7
                goids <- lookUp(ls(annaffyGO)[1], "annaffy", "GO")
                if(!is.na(goids)){
                    # GO id of the first element
                    goids[[1]]$GOID
                    # Ontology category of the first element
                    goids[[1]]$Ontology
                    # Evidence code
                    goids[[1]]$Evidence
                }
	}
}
\keyword{datasets}

