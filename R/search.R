aafSearchText <- function(chip, colnames, text, logic = "OR") {
	
	if (! do.call("require", list(chip)))
        stop(paste("Data library couldn't be loaded:", chip))

    environment <- paste(chip, "SYMBOL", sep="")

    probeids <- do.call("ls", list(as.name(environment)))
    
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
	
	if (! do.call("require", list(chip)))
        stop(paste("Data library couldn't be loaded:", chip))

    if (descendents)
    	environment <- paste(chip, "GO2ALLPROBES", sep="")
    else
    	environment <- paste(chip, "GO2PROBE", sep="")

    probeids <- NULL
    for (id in ids) {
		if (is.numeric(id) || length(grep("^[0-9]+$", id)))
			id <- sprintf("GO:%07i", as.integer(id))
		
		if (! do.call("exists", list(id, as.name(environment))))
			next
        probes <- do.call("get", list(id, as.name(environment)))
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

