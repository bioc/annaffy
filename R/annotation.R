.aaf.raw <- function(probeids, chip, type) {

    if (! do.call("require", list(chip)))
        stop(paste("Data library couldn't be loaded:", chip))

    environment <- paste(chip, type, sep="")

    do.call("multiget", list(probeids, as.name(environment)))
}

.aaf.character <- function(probeids, chip, type, class) {

    anns <- .aaf.raw(probeids, chip, type)
    result <- vector("list", length(probeids))
    attrs <- list(class = class)
    for(i in 1:length(probeids)) {
        ann <- anns[[i]]
        if( is.na(ann[1]) )
            ann <- character(0)
        attributes(ann) <- attrs
        result[[i]] <- ann
    }
    class(result) <- "aafList"
    
    return(result)
}

.aaf.integer <- function(probeids, chip, type, class) {

    anns <- .aaf.raw(probeids, chip, type)
    result <- vector("list", length(probeids))
    attrs <- list(class = class)
    for(i in 1:length(probeids)) {
        ann <- as.integer(anns[[i]])
        if( is.na(ann[1]) )
            ann <- integer(0)
        attributes(ann) <- attrs
        result[[i]] <- ann
    }
    class(result) <- "aafList"
    
    return(result)
}

.aaf.goterm <- function (num) {

    if (!nchar(num))
        return(list())
    
    if (exists(num, GOBPID2TERM))
        return(list(name = get(num, GOBPID2TERM), type = "Biological Process"))
    
    if (exists(num, GOCCID2TERM))
        return(list(name = get(num, GOCCID2TERM), type = "Cellular Component"))
    
    if (exists(num, GOMFID2TERM))
        return(list(name = get(num, GOMFID2TERM), type = "Molecular Function"))
    
    return(list())
}

aaf.handler <- function (probeids, chip, name)
{
# This function keeps track of all the types of annotation data that can
# currently be handled. If called with no arguments, it returns a list of the
# names of annotation data that can be fetched. Otherwise, it dispatches the
# request to the appropriate handler function.

    if (missing(probeids))
	c("Probe", "Symbol", "Description", "Function", "Chromosome",
	  "Chromosome Location", "GenBank", "LocusLink", "Cytoband",
	  "UniGene", "PubMed", "Gene Ontology", "Pathway")
    else
        switch(name,
               Probe = aafProbe(probeids),
               Symbol = aafSymbol(probeids, chip),
               Description = aafDescription(probeids, chip),
               Function = aafFunction(probeids, chip),
               Chromosome = aafChromosome(probeids, chip),
               "Chromosome Location" = aafChromLoc(probeids, chip),
               GenBank = aafGenBank(probeids, chip),
               LocusLink = aafLocusLink(probeids, chip),
               Cytoband = aafCytoband(probeids, chip),
               UniGene = aafUniGene(probeids, chip),
               PubMed = aafPubMed(probeids, chip),
               "Gene Ontology" = aafGO(probeids, chip),
               Pathway = aafPathway(probeids, chip))
}

.initAnnotation <- function(where) {

## Set generic methods

    if( !isGeneric("getText") )
        setGeneric("getText", function(object) standardGeneric("getText"),
                   where = where )
    
    if( !isGeneric("getURL") )
        setGeneric("getURL", function(object) standardGeneric("getURL"),
                   where = where )
    
    if( !isGeneric("getHTML") )
        setGeneric("getHTML", function(object) standardGeneric("getHTML"),
                   where = where )
    
    if( !isGeneric("getTD") )
        setGeneric("getTD", function(object) standardGeneric("getTD"),
                   where = where )
    
    if( !isGeneric("getCSS") )
        setGeneric("getCSS", function(object) standardGeneric("getCSS"),
                   where = where )

## Define methods for vector class

    setMethod("getText", "ANY", function(object) {
    
        if( !length(object) )
            return("")
        return(paste(object, collapse = ", "))
    
    }, where = where)
    
    setMethod("getURL", "ANY", function(object) {
    
        return(character(0))
    
    }, where = where)
    
    setMethod("getHTML", "ANY", function(object) {
        
        if( is.double(object) )
            object <- signif(object, getOption("sigfigs"))
        if( !nchar(text <- getText(object)) )
            return("")
        if( length(url <- getURL(object)) )
            return(paste(paste("<a href=\"", url, "\">", text, "</a>", sep = ""), collapse = " "))
        else
            return(text)
        
    }, where = where)
    
    setMethod("getTD", "ANY", function(object) {
        
        html <- getHTML(object)
        if (!nchar(html))
           html <- "&nbsp;"
        
        return(paste("<td class=\"", class(object), "\">", html, "</td>", sep = ""))       
    
    }, where = where)
    
    setMethod("getCSS", "ANY", function(object) {
        
        return(character(0))       
    
    }, where = where)

## Define class aafList
    
    setClass("aafList", "list", prototype = list(), 
             where = where)
    
    setMethod("getText", "aafList", function(object) {
        
        if( !length(object) )
            return(character(0))
        result <- character(length(object))
        for(i in 1:length(object))
            result[i] <- getText(object[[i]])
        return(result)
        
    }, where = where)
    
    setMethod("getURL", "aafList", function(object) {
    
        result <- character(0)
        for (i in 1:length(object))
            result <- c(result, getURL(object[[i]]))
        return(result)
    
    }, where = where)
    
    setMethod("getHTML", "aafList", function(object) {
        
        if( !length(object) )
            return(character(0))
        result <- character(length(object))
        for(i in 1:length(object))
            result[i] <- getHTML(object[[i]])
        return(result)
        
    }, where = where)
    
    setMethod("getTD", "aafList", function(object) {
        
        if( !length(object) )
            return(character(0))
        result <- character(length(object))
        for(i in 1:length(object))
            result[i] <- getTD(object[[i]])
        return(result)
        
    }, where = where)
    
    setMethod("getCSS", "aafList", function(object) {
        
        return(getCSS(object[[1]]))
    
    }, where = where)
    
    setMethod("[", "aafList", function(x, i, j, ..., drop = F) {
    
        result <- x@.Data[i]
        class(result) <- class(x)
        return(result)
        
    }, where=where)
    
    setMethod("show", "aafList", function(object) {
    
        frame <- parent.frame()
        if( exists("showHistory", frame) )
            history <- get("showHistory", frame)
        else
            history <- integer(0)
        
        cat("An object of class \"", class(object), "\"\n", sep = "")
        if( length(object) )
            for(i in 1:length(object)) {
                showHistory <- c(history, i)
                cat("[[", paste(showHistory, collapse = "]][["), "]]\n", sep = "")
                show(object[[i]])
                cat("\n")
            }
        else
            cat("list()\n", sep = "")
    
    }, where = where)

## Define class aafProbe
    
    setClass("aafProbe", "character", prototype = character(0), 
             where = where)
    
    assign("aafProbe", function(probeids) {
        
        probes <- as.list(probeids)
        for(i in 1:length(probes))
            class(probes[[i]]) <- "aafProbe"
        
        return(new("aafList", probes))
    
    }, envir = where)
    
    setMethod("getURL", "aafProbe", function(object) {
        
        url <- "https://www.affymetrix.com/LinkServlet?&probeset="
        
        if( !length(object) )
            return(character(0))
        return(paste(url, object, sep = ""))
        
    }, where = where)
    
## Define class aafSymbol
    
    setClass("aafSymbol", "character", prototype = character(0), 
             where = where)
    
    assign("aafSymbol", function(probeids, chip) {
        
        return(.aaf.character(probeids, chip, "SYMBOL", "aafSymbol"))
    
    }, envir = where)
    
## Define class aafDescription
    
    setClass("aafDescription", "character", prototype = character(0), 
             where = where)
    
    assign("aafDescription", function(probeids, chip) {
        
        return(.aaf.character(probeids, chip, "GENENAME", "aafDescription"))
    
    }, envir = where)
    
## Define class aafFunction
    
    setClass("aafFunction", "character", prototype = character(0), 
             where = where)
    
    assign("aafFunction", function(probeids, chip) {
        
        return(.aaf.character(probeids, chip, "SUMFUNC", "aafFunction"))
    
    }, envir = where)
    
## Define class aafChromosome
    
    setClass("aafChromosome", "character", prototype = character(0), 
             where = where)
    
    assign("aafChromosome", function(probeids, chip) {
        
        return(.aaf.character(probeids, chip, "CHR", "aafChromosome"))
    
    }, envir = where)
    
## Define class aafChromLoc
    
    setClass("aafChromLoc", "integer", prototype = integer(0), 
             where = where)
    
    assign("aafChromLoc", function(probeids, chip) {
        
        return(.aaf.integer(probeids, chip, "CHRLOC", "aafChromLoc"))
    
    }, envir = where)
    
## Define class aafGenBank
    
    setClass("aafGenBank", "character", prototype = character(0), 
             where = where)
    
    assign("aafGenBank", function(probeids, chip) {
        
        return(.aaf.character(probeids, chip, "ACCNUM", "aafGenBank"))
    
    }, envir = where)
    
    setMethod("getURL", "aafGenBank", function(object) {
    
        url <- "http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?cmd=search&db=nucleotide&term="
        urlsuffix <- "%5BACCN%5D&doptcmdl=GenBank"
        
        if( !length(object) )
            return(character(0))
        return(paste(url, object, urlsuffix, sep = ""))
    
    }, where = where)

## Define class aafLocusLink
    
    setClass("aafLocusLink", "integer", prototype = integer(0), 
             where = where)
    
    assign("aafLocusLink", function(probeids, chip) {
        
        return(.aaf.integer(probeids, chip, "LOCUSID", "aafLocusLink"))
    
    }, envir = where)
    
    setMethod("getURL", "aafLocusLink", function(object) {
        
        url <- "http://www.ncbi.nlm.nih.gov/LocusLink/LocRpt.cgi?l="
        
        if( !length(object) )
            return(character(0))
        return(paste(url, object, sep = ""))
        
    }, where = where)
    
## Define class aafCytoband
    
    setClass("aafCytoband", representation(band = "character",
                                           genbank = "character"),
             prototype = list(band = character(0),
                              genbank = character(0)), where = where)
    
    assign("aafCytoband", function(probeids, chip) {
        
        band <- .aaf.raw(probeids, chip, "MAP")
        genbank <- .aaf.raw(probeids, chip, "ACCNUM")
        result <- vector("list", length(probeids))
        navals <- is.na(band)
        result[which(navals)] <- list(new("aafCytoband"))
        result[which(!navals)] <- list(list())
        for(i in which(!navals))
            attributes(result[[i]]) <- list(band = band[[i]], genbank = genbank[[i]], class = "aafCytoband")
        class(result) <- "aafList"
        
        return(result)
        
    }, envir = where)
    
    setMethod("getText", "aafCytoband", function(object) {
    
        if( !length(object@band) )
            return("")
        return(object@band)
    
    }, where = where)
    
    setMethod("getURL", "aafCytoband", function(object) {
    
        url <- "http://www.ncbi.nlm.nih.gov/mapview/map_search.cgi?direct=on&query="
        urlsuffix <- "%5BACCN%5D"
        
        if( !length(object@band) )
            return(character(0))
        return(paste(url, object@genbank, urlsuffix, sep = ""))
    
    }, where = where)
    
    setMethod("show", "aafCytoband", function(object) {
        
        cat("An object of class \"aafCytoband\"\n")
        cat("@band    ", object@band, "\n", sep = "\"")
        cat("@genbank ", object@genbank, "\n", sep = "\"")
        
    }, where = where)

## Define class aafUniGene
    
    setClass("aafUniGene", "character", prototype = character(0), 
             where = where)
    
    assign("aafUniGene", function(probeids, chip) {
        
        return(.aaf.character(probeids, chip, "UNIGENE", "aafUniGene"))
    
    }, envir = where)
    
    setMethod("getURL", "aafUniGene", function(object) {
        
        url <- "http://www.ncbi.nlm.nih.gov/UniGene/clust.cgi?ORG="
        urlinter <- "&CID="
        
        if( !length(object) )
            return(character(0))
        return(paste(url, sub("[.]", urlinter, object), sep = ""))
        
    }, where = where)

## Define class aafPubMed
    
    setClass("aafPubMed", "integer", prototype = integer(0), 
             where = where)
    
    assign("aafPubMed", function(probeids, chip) {
        
        return(.aaf.integer(probeids, chip, "PMID", "aafPubMed"))
    
    }, envir = where)
    
    setMethod("getURL", "aafPubMed", function(object) {
        
        url <- "http://www.ncbi.nih.gov/entrez/query.fcgi?tool=bioconductor&cmd=Retrieve&db=PubMed&list_uids="
        
        if( !length(object) )
            return(character(0))
        return(paste(url, paste(object, collapse = "%2c"), sep = ""))
        
    }, where = where)
    
    setMethod("getHTML", "aafPubMed", function(object) {
        
        if( !length(object) )
            return("")
        return(paste("<a href=\"", getURL(object), "\">", length(object), "</a>", sep = ""))
        
    }, where = where)
    
    setMethod("getCSS", "aafPubMed", function(object) {
        
        return("td.aafPubMed { text-align: center }")       
    
    }, where = where)
    
## Define class aafGO
    
    setClass("aafGO", "aafList", prototype = list(), 
             where = where)
    
    assign("aafGO", function(probeids, chip) {
        
        gos <- .aaf.raw(probeids, chip, "GO")
        results <- vector("list", length(probeids))
        attrs <- list(class = "aafGO")
        for(i in 1:length(probeids)) {
            go <- gos[[i]]
            results[[i]] <- list()
            if( !is.na(go[1]) ) {
                for(j in 1:length(go)) {
                    nametype <- .aaf.goterm(go[j])
                    if( length(nametype) ) {
                        result <- list()
                        attributes(result) <- list(id = go[j], name = nametype$name, type = nametype$type, evid = names(go)[j], class = "aafGOItem")
                        results[[i]] <- c(results[[i]], list(result))
                    }
                }
            }
            attributes(results[[i]]) <- attrs
        }
        class(results) <- "aafList"
        
        return(results)
        
    }, envir = where)
    
    setMethod("getText", "aafGO", function(object) {
    
        result = callNextMethod()
        return(paste(result, collapse = ", "))
    
    }, where = where)
    
    setMethod("getURL", "aafGO", function(object) {
    
        url <- "http://godatabase.org/cgi-bin/go.cgi?open_0="
        
        if( !length(object) )
            return(character(0))
        url <- paste(url, object[[1]]@id, sep = "")
        for(i in 2:length(object))
            url <- paste(url, object[[i]]@id, sep = "&open_0=")
        return(url)
    
    }, where = where)
    
    setMethod("getHTML", "aafGO", function(object) {
    
        result = callNextMethod()
        return(paste(result, collapse = " "))
    
    }, where = where)
    
    setMethod("getTD", "aafGO", function(object) {
        
        html <- getHTML(object)
        if (!nchar(html))
           html <- "&nbsp;"
        
        return(paste("<td class=\"", class(object), "\">", html, "</td>", sep = ""))       
    
    }, where = where)
    
    setMethod("getCSS", "aafGO", function(object) {
        
        return("p.aafGOItem { margin-top: 1px; margin-bottom: 1px; padding-left: 10px; text-indent: -10px }")       
    
    }, where = where)

## Define class aafGOItem
    
    setClass("aafGOItem", representation(id = "character",
                                         name = "character",
                                         type = "character",
                                         evid = "character"),
             prototype = list(id = character(0),
                              name = character(0),
                              type = character(0),
                              evid = character(0)), where = where)
    
    setMethod("getText", "aafGOItem", function(object) {
    
        if( !length(object@id) )
            return("")
        return(paste(object@id, ": ", object@name, sep = ""))
    
    }, where = where)
    
    setMethod("getURL", "aafGOItem", function(object) {
    
        url <- "http://godatabase.org/cgi-bin/go.cgi?open_0="
        
        if( !length(object@id) )
            return(character(0))
        return(paste(url, object@id, sep = ""))
    
    }, where = where)
    
    setMethod("getHTML", "aafGOItem", function(object) {
        
        if( !length(object@id) )
            return("")
        return(paste("<p class=\"aafGOItem\"><a href=\"", getURL(object), "\" title=\"", object@type, " (", object@evid, ")\">", object@name, "</a></p>", sep = ""))
        
    }, where = where)
    
    setMethod("show", "aafGOItem", function(object) {
        
        cat("An object of class \"aafGOItem\"\n")
        cat("@id   ", object@id, "\n", sep = "\"")
        cat("@name ", object@name, "\n", sep = "\"")
        cat("@type ", object@type, "\n", sep = "\"")
        cat("@evid ", object@evid, "\n", sep = "\"")
        
    }, where = where)

## Define class aafPathway
    
    setClass("aafPathway", "aafList", prototype = list(), 
             where = where)
    
    assign("aafPathway", function(probeids, chip) {
        
        pathways <- .aaf.raw(probeids, chip, "PATH")
        enzymes <- .aaf.raw(probeids, chip, "ENZYME")
        results <- vector("list", length(probeids))
        attrs <- list(class = "aafPathway")
        for(i in 1:length(probeids)) {
            pathway <- pathways[[i]]
            if( is.na(pathway[1]) ) {
                results[[i]] <- list()
            }
            else {
                name <- multiget(pathway, KEGGPATHID2NAME)
                enzyme <- enzymes[[i]][1]
                if( is.na(enzyme) )
                    enzyme <- character(0)
                result <- vector("list", length(pathway))
                for(j in 1:length(pathway)) {
                    result[[j]] <- list()
                    attributes(result[[j]]) <- list(id = pathway[j], name = name[[j]], enzyme = enzyme, class = "aafPathwayItem")
                }
                results[[i]] <- result
            }
            attributes(results[[i]]) <- attrs
        }
        class(results) <- "aafList"
        
        return(results)
        
    }, envir = where)
    
    setMethod("getText", "aafPathway", function(object) {
    
        result = callNextMethod()
        return(paste(result, collapse = ", "))
    
    }, where = where)
    
    setMethod("getHTML", "aafPathway", function(object) {
    
        result = callNextMethod()
        return(paste(result, collapse = " "))
    
    }, where = where)
    
    setMethod("getTD", "aafPathway", function(object) {
        
        html <- getHTML(object)
        if (!nchar(html))
           html <- "&nbsp;"
        
        return(paste("<td class=\"", class(object), "\">", html, "</td>", sep = ""))       
    
    }, where = where)
    
    setMethod("getCSS", "aafPathway", function(object) {
        
        return("p.aafPathwayItem { margin-top: 1px; margin-bottom: 1px; padding-left: 10px; text-indent: -10px }")       
    
    }, where = where)

## Define class aafPathwayItem
    
    setClass("aafPathwayItem", representation(id = "character",
                                           name = "character",
                                           enzyme = "character"),
             prototype = list(id = character(0),
                              name = character(0),
                              enzyme = character(0)), where = where)
    
    setMethod("getText", "aafPathwayItem", function(object) {
    
        if( !length(object@id) )
            return("")
        return(paste(object@id, ": ", object@name, sep = ""))
    
    }, where = where)
    
    setMethod("getURL", "aafPathwayItem", function(object) {
        
        url <- "http://www.genome.ad.jp/dbget-bin/show_pathway?MAP"
        urlnoenzyme <- "http://www.genome.ad.jp/kegg/pathway/hsa/hsa"
        
        if( !length(object@id) )
            return(character(0))
        if( length(object@enzyme) )
            return(paste(url, object@id, "+", object@enzyme, sep = ""))
        return(paste(urlnoenzyme, object@id, ".html", sep = ""))
        
    }, where = where)
    
    setMethod("getHTML", "aafPathwayItem", function(object) {
        
        if( !length(object@id) )
            return("")
        return(paste("<p class=\"aafPathwayItem\"><a href=\"", getURL(object), "\">", object@name, "</a></p>", sep = ""))
        
    }, where = where)
    
    setMethod("show", "aafPathwayItem", function(object) {
        
        cat("An object of class \"aafGOItem\"\n")
        cat("@id     ", object@id, "\n", sep = "\"")
        cat("@name   ", object@name, "\n", sep = "\"")
        cat("@enzyme ", object@enzyme, "\n", sep = "\"")
        
    }, where = where)
}
