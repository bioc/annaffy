is.annpkg <- function(packages, lib.loc = NULL) {
    
    envnames <- c("ACCNUM", "CHR", "CHRLOC", "ENZYME", "GENENAME", "GO",
                  "ENTREZID", "MAP", "PATH", "PMID", "SYMBOL", "UNIGENE")
    
    result <- logical(length(packages))
    
    for (i in seq(along = packages)) {
        wasLoaded <- FALSE
        loadOk <- FALSE
        pkgSearchNm <- paste("package", packages[i], sep=":")
        if (pkgSearchNm %in% search()) {
            wasLoaded <- TRUE
            loadOk <- TRUE
        }
        if (!wasLoaded)
          loadOk <- suppressWarnings(require(packages[i],
                                             character.only=TRUE,
                                             quietly=TRUE))
        if (!loadOk)
          stop("package ", packages[i], " is not available")
        pkgEnv <- as.environment(pkgSearchNm)
        prefix <- annpkg_prefix(packages[i])
        entries <- sub(prefix, "", grep(prefix, ls(pkgEnv), value=TRUE))
        result[i] <- all(envnames %in% entries)
        if (!wasLoaded)
          detach(pos=match(pkgSearchNm, search()))
    }
    
    result
}

is_dbpackage <- function(chip) {
    pkgEnv <- as.environment(paste("package", chip, sep=":"))
    exists("db_conn", pkgEnv)
}

dbpackage_prefix <- function(chip) {
    substr(chip, 1, nchar(chip) - 2)
}

annpkg_prefix <- function(chip) {
    if (is_dbpackage(chip))
      dbpackage_prefix(chip)
    else
      chip
}


.aaf.raw <- function(probeids, chip, type) {

    chkPkgs(chip)

    require(chip, character.only = TRUE) ||
        stop(paste("Couldn't load data package", chip))
    chip <- annpkg_prefix(chip)
    environment <- paste(chip, type, sep="")

    do.call("mget", list(probeids, as.name(environment), ifnotfound=NA))
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
        if (all(is.na(anns[[i]])))
            ann <- integer(0)
        else if (is.numeric(anns[[i]]))
            ann <- as.integer(anns[[i]])
        else {
            tmp <- suppressWarnings(as.integer(unlist(strsplit(anns[[i]], ";"))))
            ## Needed because yeast Pub Med data is returned as a ";" separated
            ## string, often with a trailing NA. Suppressed warning of coersion
            ## of "NA" to NA. AFAIK, this doesn't affect other Pub Med data.
            ann <- tmp[!is.na(tmp)]
        }
        attributes(ann) <- attrs
        result[[i]] <- ann
    }
    class(result) <- "aafList"

    return(result)
}

.aaf.goterm <- function (num) {

    if ( !is.list(num) )
        return(list())

    if (is.na(num$Ontology))
        return(list())

    if (!exists(num$GOID, GOTERM))
        return(list())

    if (num$Ontology == "BP")
        return(list(name = attr(get(num$GOID, GOTERM), "Term"),
                    type = "Biological Process"))

    if (num$Ontology == "CC")
        return(list(name = attr(get(num$GOID, GOTERM), "Term"),
                    type = "Cellular Component"))

    if (num$Ontology == "MF")
        return(list(name = attr(get(num$GOID, GOTERM), "Term"),
                    type = "Molecular Function"))

    return(list())
}

aaf.handler <- function (probeids, chip, name)
{
# This function keeps track of all the types of annotation data that can
# currently be handled. If called with no arguments, it returns a list of the
# names of annotation data that can be fetched. Otherwise, it dispatches the
# request to the appropriate handler function.

    deps <- list(
                 "Probe" = character(0),
                 "Symbol" = "SYMBOL",
                 "Description" = "GENENAME",
                 "Function" = "SUMFUNC",
                 "Chromosome" = "CHR",
                 "Chromosome Location" = "CHRLOC",
                 "GenBank" = "ACCNUM",
                 "LocusLink" = "ENTREZID",
                 "Cytoband" = c("MAP", "ACCNUM"),
                 "UniGene" = "UNIGENE",
                 "PubMed" = "PMID",
                 "Gene Ontology" = "GO",
                 "Pathway" = c("PATH", "ENZYME")
                 )
    if (!missing(chip)) {
        require(chip, character.only = TRUE) ||
        stop(paste("Couldn't load data package", chip))
        use <- rep(TRUE, length(deps))
        pkgSyms <- ls(paste("package:", chip, sep=""))
        prefix <- annpkg_prefix(chip)
        for (i in seq(along = deps)) {
            if (any(!(paste(prefix, deps[[i]], sep="") %in% pkgSyms)))
              use[i] <- FALSE
        }
        deps <- deps[use]
    }
    if( missing(probeids) )
        return(names(deps))
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

## Set generic methods

if( !isGeneric("getText") )
    setGeneric("getText", function(object) standardGeneric("getText"))

if( !isGeneric("getURL") )
    setGeneric("getURL", function(object) standardGeneric("getURL"))

if( !isGeneric("getHTML") )
    setGeneric("getHTML", function(object) standardGeneric("getHTML"))

if( !isGeneric("getTD") )
    setGeneric("getTD", function(object) standardGeneric("getTD"))

if( !isGeneric("getCSS") )
    setGeneric("getCSS", function(object) standardGeneric("getCSS"))

## Define methods for vector class

setMethod("getText", "ANY", function(object) {

    if( !length(object) )
        return("")
    return(paste(object, collapse = ", "))
})

setMethod("getURL", "ANY", function(object) {

    return(character(0))
})

setMethod("getHTML", "ANY", function(object) {

    if( is.double(object) )
        object <- signif(object, ifelse(is.null(getOption("sigfigs")), 6, 
                                                getOption("sigfigs")))
    if( !nchar(text <- getText(object)) )
        return("")
    if( length(url <- getURL(object)) )
        return(paste(paste("<a href=\"", url, "\">", text, "</a>", sep = ""), collapse = " "))
    else
        return(text)
})

setMethod("getTD", "ANY", function(object) {

    html <- getHTML(object)
    if (!nchar(html))
       html <- "&nbsp;"

    return(paste("<td class=\"", class(object), "\">", html, "</td>", sep = ""))
})

setMethod("getCSS", "ANY", function(object) {

    return(character(0))
})

## Define class aafList

setClass("aafList", "list", prototype = list())

setMethod("getText", "aafList", function(object) {

    if( !length(object) )
        return(character(0))
    result <- character(length(object))
    for(i in 1:length(object))
        result[i] <- getText(object[[i]])
    return(result)
})

setMethod("getURL", "aafList", function(object) {

    result <- character(0)
    for (i in 1:length(object))
        result <- c(result, getURL(object[[i]]))
    return(result)
})

setMethod("getHTML", "aafList", function(object) {

    if( !length(object) )
        return(character(0))
    result <- character(length(object))
    for(i in 1:length(object))
        result[i] <- getHTML(object[[i]])
    return(result)
})

setMethod("getTD", "aafList", function(object) {

    if( !length(object) )
        return(character(0))
    result <- character(length(object))
    for(i in 1:length(object))
        result[i] <- getTD(object[[i]])
    return(result)
})

setMethod("getCSS", "aafList", function(object) {

    return(getCSS(object[[1]]))
})

setMethod("[", "aafList", function(x, i, j, ..., drop = F) {

    result <- x@.Data[i]
    class(result) <- class(x)
    return(result)
})

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
})

## Define class aafProbe

setClass("aafProbe", "character", prototype = character(0))

aafProbe <- function(probeids) {

    probes <- as.list(probeids)
    for(i in 1:length(probes))
        class(probes[[i]]) <- "aafProbe"

    return(new("aafList", probes))
}

setMethod("getURL", "aafProbe", function(object) {

    url <- "https://www.affymetrix.com/LinkServlet?&probeset="

    if( !length(object) )
        return(character(0))
    return(paste(url, object, sep = ""))
})

## Define class aafSymbol

setClass("aafSymbol", "character", prototype = character(0))

aafSymbol <- function(probeids, chip) {

    return(.aaf.character(probeids, chip, "SYMBOL", "aafSymbol"))
}

## Define class aafDescription

setClass("aafDescription", "character", prototype = character(0))

aafDescription <- function(probeids, chip) {

    return(.aaf.character(probeids, chip, "GENENAME", "aafDescription"))
}

## Define class aafFunction

setClass("aafFunction", "character", prototype = character(0))

aafFunction <- function(probeids, chip) {

    return(.aaf.character(probeids, chip, "SUMFUNC", "aafFunction"))
}

## Define class aafChromosome

setClass("aafChromosome", "character", prototype = character(0))

aafChromosome <- function(probeids, chip) {

    return(.aaf.character(probeids, chip, "CHR", "aafChromosome"))
}

## Define class aafChromLoc

setClass("aafChromLoc", "integer", prototype = integer(0))

aafChromLoc <- function(probeids, chip) {

    return(.aaf.integer(probeids, chip, "CHRLOC", "aafChromLoc"))
}

## Define class aafGenBank

setClass("aafGenBank", "character", prototype = character(0))

aafGenBank <- function(probeids, chip) {

    return(.aaf.character(probeids, chip, "ACCNUM", "aafGenBank"))
}

setMethod("getURL", "aafGenBank", function(object) {

    url <- "http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?cmd=search&db=nucleotide&term="
    urlsuffix <- "%5BACCN%5D&doptcmdl=GenBank"

    if( !length(object) )
        return(character(0))
    return(paste(url, object, urlsuffix, sep = ""))
})

## Define class aafLocusLink

setClass("aafLocusLink", "integer", prototype = integer(0))

aafLocusLink <- function(probeids, chip) {

    return(.aaf.integer(probeids, chip, "ENTREZID", "aafLocusLink"))
}

setMethod("getURL", "aafLocusLink", function(object) {

    url <- "http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?db=gene&cmd=Retrieve&dopt=Graphics&list_uids="

    if( !length(object) )
        return(character(0))
    return(paste(url, object, sep = ""))
})

## Define class aafCytoband

setClass("aafCytoband", representation(band = "character",
                                       genbank = "character"),
         prototype = list(band = character(0),
                          genbank = character(0)))

aafCytoband <- function(probeids, chip) {

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
}

setMethod("getText", "aafCytoband", function(object) {

    if( !length(object@band) )
        return("")
    return(object@band)
})

setMethod("getURL", "aafCytoband", function(object) {

    url <- "http://www.ncbi.nlm.nih.gov/mapview/map_search.cgi?direct=on&query="
    urlsuffix <- "%5BACCN%5D"

    if( !length(object@band) )
        return(character(0))
    return(paste(url, object@genbank, urlsuffix, sep = ""))
})

setMethod("show", "aafCytoband", function(object) {

    cat("An object of class \"aafCytoband\"\n")
    cat("@band    ", object@band, "\n", sep = "\"")
    cat("@genbank ", object@genbank, "\n", sep = "\"")
})

## Define class aafUniGene

setClass("aafUniGene", "character", prototype = character(0))

aafUniGene <- function(probeids, chip) {

    return(.aaf.character(probeids, chip, "UNIGENE", "aafUniGene"))
}

setMethod("getURL", "aafUniGene", function(object) {

    url <- "http://www.ncbi.nlm.nih.gov/UniGene/clust.cgi?ORG="
    urlinter <- "&CID="

    if( !length(object) )
        return(character(0))
    return(paste(url, sub("[.]", urlinter, object), sep = ""))
})

setMethod("getHTML", "aafUniGene", function(object) {

    if( !length(object) )
        return("")
    if( length(url <- getURL(object)) )
        return(paste(paste("<a href=\"", url, "\">", object, "</a>", sep = ""), collapse = " "))
    else
        return(text)
})

## Define class aafPubMed

setClass("aafPubMed", "integer", prototype = integer(0))

aafPubMed <- function(probeids, chip) {

    return(.aaf.integer(probeids, chip, "PMID", "aafPubMed"))
}

setMethod("getURL", "aafPubMed", function(object) {

    url <- "http://www.ncbi.nih.gov/entrez/query.fcgi?tool=bioconductor&cmd=Retrieve&db=PubMed&list_uids="

    if( !length(object) )
        return(character(0))
    return(paste(url, paste(object, collapse = "%2c"), sep = ""))
})

setMethod("getHTML", "aafPubMed", function(object) {

    if( !length(object) )
        return("")
    return(paste("<a href=\"", getURL(object), "\">", length(object), "</a>", sep = ""))
})

setMethod("getCSS", "aafPubMed", function(object) {

    return("td.aafPubMed { text-align: center }")
})

## Define class aafGO

setClass("aafGO", "aafList", prototype = list())

aafGO <- function(probeids, chip) {

    gos <- .aaf.raw(probeids, chip, "GO")
    results <- vector("list", length(probeids))
    attrs <- list(class = "aafGO")
    for(i in 1:length(probeids)) {
        go <- gos[[i]]
        results[[i]] <- list()
        if( is.list(go) ) {
            for(j in 1:length(go)) {
                nametype <- .aaf.goterm(go[[j]])
                if( length(nametype) ) {
                    result <- list()
                    attributes(result) <- list(id = go[[j]]$GOID, name = nametype$name, type = nametype$type, evid = go[[j]]$Evidence, class = "aafGOItem")
                    results[[i]] <- c(results[[i]], list(result))
                }
            }
        }
        attributes(results[[i]]) <- attrs
    }
    class(results) <- "aafList"

    return(results)
}

setMethod("getText", "aafGO", function(object) {

    result = callNextMethod()
    return(paste(result, collapse = ", "))
})

setMethod("getURL", "aafGO", function(object) {

    url <- "http://amigo.geneontology.org/cgi-bin/amigo/go.cgi?view=query&query="

    if( !length(object) )
        return(character(0))
    url <- paste(url, object[[1]]@id, sep = "")
    for(i in 2:length(object))
        url <- paste(url, object[[i]]@id, sep = "%0a")
    return(url)
})

setMethod("getHTML", "aafGO", function(object) {

    result = callNextMethod()
    return(paste(result, collapse = " "))
})

setMethod("getTD", "aafGO", function(object) {

    html <- getHTML(object)
    if (!nchar(html))
       html <- "&nbsp;"

    return(paste("<td class=\"", class(object), "\">", html, "</td>", sep = ""))
})

setMethod("getCSS", "aafGO", function(object) {

    return("p.aafGOItem { margin-top: 1px; margin-bottom: 1px; padding-left: 10px; text-indent: -10px }")
})

## Define class aafGOItem

setClass("aafGOItem", representation(id = "character",
                                     name = "character",
                                     type = "character",
                                     evid = "character"),
         prototype = list(id = character(0),
                          name = character(0),
                          type = character(0),
                          evid = character(0)))

setMethod("getText", "aafGOItem", function(object) {

    if( !length(object@id) )
        return("")
    return(paste(object@id, ": ", object@name, sep = ""))
})

setMethod("getURL", "aafGOItem", function(object) {

    url <- "http://amigo.geneontology.org/cgi-bin/amigo/go.cgi?view=query&query="

    if( !length(object@id) )
        return(character(0))
    return(paste(url, object@id, sep = ""))
})

setMethod("getHTML", "aafGOItem", function(object) {

    if( !length(object@id) )
        return("")
    return(paste("<p class=\"aafGOItem\"><a href=\"", getURL(object), "\" title=\"", object@type, " (", object@evid, ")\">", object@name, "</a></p>", sep = ""))
})

setMethod("show", "aafGOItem", function(object) {

    cat("An object of class \"aafGOItem\"\n")
    cat("@id   ", object@id, "\n", sep = "\"")
    cat("@name ", object@name, "\n", sep = "\"")
    cat("@type ", object@type, "\n", sep = "\"")
    cat("@evid ", object@evid, "\n", sep = "\"")
})

## Define class aafPathway

setClass("aafPathway", "aafList", prototype = list())

aafPathway <- function(probeids, chip) {

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
            name <- mget(pathway, KEGGPATHID2NAME, ifnotfound=NA)
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
}

setMethod("getText", "aafPathway", function(object) {

    result = callNextMethod()
    return(paste(result, collapse = ", "))
})

setMethod("getHTML", "aafPathway", function(object) {

    result = callNextMethod()
    return(paste(result, collapse = " "))
})

setMethod("getTD", "aafPathway", function(object) {

    html <- getHTML(object)
    if (!nchar(html))
       html <- "&nbsp;"

    return(paste("<td class=\"", class(object), "\">", html, "</td>", sep = ""))
})

setMethod("getCSS", "aafPathway", function(object) {

    return("p.aafPathwayItem { margin-top: 1px; margin-bottom: 1px; padding-left: 10px; text-indent: -10px }")
})

## Define class aafPathwayItem

setClass("aafPathwayItem", representation(id = "character",
                                          name = "character",
                                          enzyme = "character"),
         prototype = list(id = character(0),
                          name = character(0),
                          enzyme = character(0)))

setMethod("getText", "aafPathwayItem", function(object) {

    if( !length(object@id) )
        return("")
    return(paste(object@id, ": ", object@name, sep = ""))
})

setMethod("getURL", "aafPathwayItem", function(object) {

    url <- "http://www.genome.ad.jp/dbget-bin/show_pathway?MAP"
    urlnoenzyme <- "http://www.genome.ad.jp/kegg/pathway/hsa/hsa"

    if( !length(object@id) )
        return(character(0))
    if( length(object@enzyme) )
        return(paste(url, object@id, "+", object@enzyme, sep = ""))
    return(paste(urlnoenzyme, object@id, ".html", sep = ""))
})

setMethod("getHTML", "aafPathwayItem", function(object) {

    if( !length(object@id) )
        return("")
    return(paste("<p class=\"aafPathwayItem\"><a href=\"", getURL(object), "\">", object@name, "</a></p>", sep = ""))
})

setMethod("show", "aafPathwayItem", function(object) {

    cat("An object of class \"aafGOItem\"\n")
    cat("@id     ", object@id, "\n", sep = "\"")
    cat("@name   ", object@name, "\n", sep = "\"")
    cat("@enzyme ", object@enzyme, "\n", sep = "\"")
})
