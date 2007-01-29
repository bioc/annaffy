aafSearchText <- function(chip, colnames, text, logic = "OR") {
	
	require(chip, character.only = TRUE) ||
        stop(paste("Couldn't load data package", chip))

    prefix <- annpkg_prefix(chip)
    environment <- paste(prefix, "GENENAME", sep="")

    probeids <- ls(get(environment))
    
    ann <- aafTableAnn(probeids, chip, colnames)
    
    matches <- NULL
    for (col in colnames) {
    	coltext <- getText(ann[[col]])
    	for (tex in text) {
    	    match <- grep(tex, coltext, ignore.case = TRUE)
    	    if (logic == "OR" || is.null(matches))
    	        matches <- union(matches, match)
    	    else
    	        matches <- intersect(matches, match)
    	}
    }
    matches <- sort(matches)
    
    return(probeids[matches])
}

aafSearchGO <- function(chip, ids, descendents = TRUE, logic = "OR") {
	
	require(chip, character.only = TRUE) ||
        stop(paste("Couldn't load data package", chip))

    prefix <- annpkg_prefix(chip)
    if (descendents)
    	environment <- paste(prefix, "GO2ALLPROBES", sep="")
    else
    	environment <- paste(prefix, "GO2PROBE", sep="")

    probeids <- NULL
    environment <- get(environment)
    for (id in ids) {
		if (is.numeric(id) || length(grep("^[0-9]+$", id)))
			id <- sprintf("GO:%07i", as.integer(id))
		
		if (! exists(id, environment))
			next
        probes <- get(id, environment)
        if (length(probes) == 1 && is.na(probes))
            next
        if (logic == "OR" || is.null(probeids))
            probeids <- union(probeids, probes)
        else
            probeids <- intersect(probeids, probes)
    }
    
    if (is.null(probeids) || length(probeids) == 1 && is.na(probeids))
        return(character())
    
    return(probeids)
}

