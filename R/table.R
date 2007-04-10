## Define class aafIntensity

setClass("aafIntensity", "numeric", prototype = numeric(0))

setMethod("getTD", "aafIntensity", function(object) {
    
    min <- getOption("minIntensity")
    max <- getOption("maxIntensity")
    color <- 100*(1-(object-min)/(max-min))
    style <- paste("background-color: rgb(", signif(color,3), "%, 100%, ", signif(color,3), "%)", sep = "")
    
    return(paste("<td class=\"", class(object), "\" style=\"", style, "\">", getHTML(object), "</td>", sep = ""))       
})

## Define class aafSigned

setClass("aafSigned", "numeric", prototype = numeric(0))

setMethod("getTD", "aafSigned", function(object) {
    
    if (object > 0)
        class <- "aafSignedPos"
    else if (object < 0)
        class <- "aafSignedNeg"
    else
        class <- "aafSignedZero"
    
    return(paste("<td class=\"", class, "\">", getHTML(object), "</td>", sep = ""))       
})

setMethod("getCSS", "aafSigned", function(object) {
    
    return(c("td.aafSignedPos { background-color: #ccf }",
             "td.aafSignedNeg { background-color: #fcc }"))
})

## Set generic methods

if( !isGeneric("probeids") )
    setGeneric("probeids", function(object) standardGeneric("probeids"))

if( !isGeneric("probeids<-") )
    setGeneric("probeids<-", function(object, value) standardGeneric("probeids<-"))

if( !isGeneric("colnames") )
    setGeneric("colnames", function(x, do.NULL = TRUE, prefix = "col") standardGeneric("colnames"))

if( !isGeneric("colnames<-") )
    setGeneric("colnames<-", function(x, value) standardGeneric("colnames<-"))

if( !isGeneric("saveHTML") )
    setGeneric("saveHTML", function(object, ...) standardGeneric("saveHTML"))

if( !isGeneric("saveText") )
    setGeneric("saveText", function(object, ...) standardGeneric("saveText"))

## Define class aafTable

setClass("aafTable", representation(probeids = "character",
                                    table = "list"),
         prototype = list(probeids = character(0),
                          table = list()))

## Constructors for aafTable
                          
aafTable <- function(..., items = list(...), colnames = names(items), 
                     probeids = character(0), signed = FALSE) {
    
    len <- length(items[[1]])
    
    if (is.null(colnames))
        stop("Column names must be provided")
    
    if (sum(duplicated(colnames)))
        stop("All column names must be unique")
    
    if (length(items) != length(colnames))
        stop("There must be the same number of column names as columns")
        
    for (col in colnames)
        if (!nchar(col))
            stop("Blank column names not allowed")
    
    for (item in items)
        if (length(item) != len)
            stop("All columns must be of equal length")
    
    table <- vector("list", length(items))
    for (col in 1:length(items)) {
        if (class(items[[col]]) == "aafList")
            table[[col]] <- items[[col]]
        else
            table[[col]] <- new("aafList", as.list(items[[col]]))
        if (signed)
            for (row in 1:len)
                class(table[[col]][[row]]) <- "aafSigned"
    }
    names(table) <- colnames
    
    return(new("aafTable", probeids = probeids, table = table))
}

aafTableFrame <- function(frame, colnames = names(frame), 
                          probeids = row.names(frame), signed = FALSE) {

    len <- dim(frame)[1]
    
    if (sum(duplicated(colnames)))
        stop("All column names must be unique")
    
    for (col in colnames)
        if (!nchar(col))
            stop("Blank column names not allowed")
    
    table <- vector("list", dim(frame)[2])
    for (col in 1:dim(frame)[2]) {
        table[[col]] <- new("aafList", as.list(frame[,col]))
        if (signed)
            for (row in 1:len)
                class(table[[col]][[row]]) <- "aafSigned"
    }
    names(table) <- colnames
    
    return(new("aafTable", probeids = probeids, table = table))
}

aafTableAnn <- function(probeids, chip, colnames = aaf.handler(chip = chip),
                        widget = FALSE) {
    
    colnames <- intersect(colnames, aaf.handler(chip = chip))
    if (widget)
        colnames <- selectorWidget(aaf.handler(), colnames, ordernsel = TRUE,
                                   title = "Select Annotation Data Columns")
    
    table <- vector("list", length(colnames))
    for (i in 1:length(colnames)) {
        table[[i]] = aaf.handler(probeids, chip, colnames[i])
    }
    names(table) = colnames
    
    return(new("aafTable", probeids = probeids, table = table))
}

aafTableInt <- function(exprSet, colnames = sampleNames(exprSet), 
                        probeids = featureNames(exprSet)) {
    
    range <- match(probeids, featureNames(exprSet))
    expr <- exprs(exprSet)
    table <- vector("list", dim(expr)[2])
    for (col in 1:length(table)) {
        table[[col]] <- as.list(as.double(expr[range,col]))
        class(table[[col]]) <- "aafList"
        for (row in 1:length(range))
            class(table[[col]][[row]]) <- "aafIntensity"
    }
    names(table) = colnames
    
    return(new("aafTable", probeids = probeids, table = table))
}

## Methods for aafTable

setMethod("probeids", "aafTable", function(object) {

    return(object@probeids)
})

setReplaceMethod("probeids", "aafTable", function(object, value) {

    if (!length(value))
        value <- character(0)
    else if (length(value) != length(object@table[[1]]))
        stop("Wrong number of probe ids")
    
    if (sum(!nchar(value)))     stop("Blank probe ids not allowed")
    if (sum(is.na(value)))      stop("NA probe ids not allowed")
    if (sum(duplicated(value))) stop("All probe ids must be unique")
    
    object@probeids <- value
    return(object)
})

setMethod("colnames", c("aafTable","missing","missing"), function(x) {

    return(names(x@table))
})

setReplaceMethod("colnames", "aafTable", function(x, value) {

    if (length(value) != length(x@table))
        stop("Wrong number of column names")
    
    if (sum(!nchar(value)))     stop("Blank column names not allowed")
    if (sum(is.na(value)))      stop("NA column names not allowed")
    if (sum(duplicated(value))) stop("All column names must be unique")
    
    names(x@table) <- value
    return(x)
})

dim.aafTable <- function(x) {

    return(c(length(x@table[[1]]), length(x@table)))
}

merge.aafTable <- function(x, y, all = FALSE, all.x = all, all.y = all, 
                           suffixes = c(".x",".y"), ...) {
    
    if (!length(y@probeids)) {
        if (length(x@table[[1]]) != length(y@table[[1]]))
            stop("The tables must have the same number of rows")
        probeids <- x@probeids
        xrange <- 1:length(x@table[[1]])
        yrange <- xrange
    } else if (!length(x@probeids)) {
        if (length(x@table[[1]]) != length(y@table[[1]]))
            stop("The tables must have the same number of rows")
        probeids <- y@probeids
        yrange <- 1:length(y@table[[1]])
        xrange <- yrange
    } else {
        if (all.x && all.y)
            probeids <- union(x@probeids, y@probeids)
        else if (all.x)
            probeids <- x@probeids
        else if (all.y)
            probeids <- y@probeids
        else {
            probeids <- intersect(x@probeids, y@probeids)
            if (!length(probeids))
                stop("The tables do not share any common probe ids")
        }
        xrange <- match(probeids, x@probeids)
        yrange <- match(probeids, y@probeids)
    }
    xrangena <- which(is.na(xrange))
    yrangena <- which(is.na(yrange))
    
    xnames <- names(x@table)
    ynames <- names(y@table)
    common <- intersect(xnames, ynames)
    xmatch <- match(common, xnames)
    ymatch <- match(common, ynames)
    xnames[xmatch] <- paste(xnames[xmatch], suffixes[1], sep = "")
    ynames[ymatch] <- paste(ynames[ymatch], suffixes[2], sep = "")
    names(x@table) <- xnames
    names(y@table) <- ynames
    
    for (col in 1:length(x@table)) {
        colclass <- class(x@table[[col]][[1]])
        x@table[[col]] <- x@table[[col]][xrange]
        x@table[[col]][xrangena] <- rep(list(new(colclass)), length(xrangena))
    }
    for (col in 1:length(y@table)) {
        colclass <- class(y@table[[col]][[1]])
        y@table[[col]] <- y@table[[col]][yrange]
        y@table[[col]][yrangena] <- rep(list(new(colclass)), length(yrangena))
    }
    
    return(new("aafTable", probeids = probeids, table = c(x@table, y@table)))
}

rbind.aafTable <- function(..., deparse.level = 1)  {
    
    tables <- list(...)
    cols <- colnames(tables[[1]])
    noprobeids <- length(probeids(tables[[1]])) == 0
    probeids <- character(0)
    table <- vector("list", dim(tables[[1]])[2])
    names(table) <- cols
    
    for (tab in tables) {
        if (!(length(cols) == length(colnames(tab)) && !sum(!(cols == colnames(tab)))))
            stop("The column names must be the same in all tables")
        if (noprobeids && length(probeids(tab)))
            stop("Tables cannot have both defined and undefined probe ids")
    }
    
    for (tab in tables) {
        if (!noprobeids)
            probeids <- c(probeids, probeids(tab))
        for (i in 1:length(cols))
            table[[i]] <- c(table[[i]], tab[[i]])
    }
    for (i in 1:length(cols))
        class(table[[i]]) <- "aafList"
    
    return(return(new("aafTable", probeids = probeids, table = table)))
}

setMethod("[", "aafTable", function(x, i, j, ..., drop = FALSE) {

    if (missing(i)) i <- 1:dim(x)[1]
    if (missing(j)) j <- 1:dim(x)[2]
    if (is.character(i)) i <- match(i, probeids(x))
    if (is.character(j)) j <- match(j, colnames(x))
    
    if (drop && length(j) == 1) {
        if (length(i) == 1)
            return(x@table[[j]][[i]])
        return(x@table[[j]][i])
    }
    
    table <- vector("list", length(j))
    for (col in 1:length(table))
        table[[col]] <- x@table[[j[col]]][i]
    names(table) <- names(x@table)[j]
    
    return(new("aafTable", probeids = x@probeids[i], table = table))
})

setMethod("[[", "aafTable", function(x, i, j, ...) {

    result <- x@table
    for (ik in i)
        result <- result[[ik]]
    
    return(result)
})

"$.aafTable" <- function(x, val) {

    return(x@table[[as.character(val)]])
}

setMethod("saveHTML", "aafTable", function(object, filename, 
                                           title = "Bioconductor Affymetrix Probe Listing",
                                           colnames = names(object@table), 
                                           range = 1:dim(object)[1],
                                           open = FALSE, widget = FALSE) {

    colnames <- intersect(colnames, names(object@table))
    if (widget)
        colnames <- selectorWidget(aaf.handler(), colnames, ordernsel = TRUE,
                                   title = "Select Columns to Save")
    
    if (is.character(range))
        range <- match(range, probeids(object))
    
    maxIntensity = 0.0;
    minIntensity = 100.0;
    for(col in colnames) {
        if (class(object@table[[col]][[1]]) == "aafIntensity") {
            maxIntensity = max(as.numeric(object@table[[col]]), maxIntensity, na.rm=TRUE)
            minIntensity = min(as.numeric(object@table[[col]]), minIntensity, na.rm=TRUE)
        }
    }
    options(maxIntensity = maxIntensity)
    options(minIntensity = minIntensity)
    
    css <- character(0)
    th <- ""
    rows <- vector("character", length(range))
    
    for(col in colnames) {
        css <- c(css, getCSS(object@table[[col]]))
        th <- paste(th, "<th>", col, "</th>\n", sep = "")
        rows <- paste(rows, getTD(object@table[[col]][range]), "\n", sep = "")
    }
    
    outfile <- file(filename, "w")
    cat("<html>", "<head>", "<title>", title, "</title>", 
        "<meta http-equiv=\"Content-Style-Type\" content=\"text/css\">", 
        "<style type=\"text/css\">", unique(css), "</style>",
        "<script language=\"JavaScript\">", "</script>", "</head>", 
        "<body bgcolor=\"#FFFFFF\">", 
        "<h1 align=\"center\">", title, "</h1>", 
        "<table border=\"2\">", file = outfile, sep = "\n")
        
    cat("<tr>\n", th, "</tr>\n", file = outfile, sep = "")
    for (i in 1:length(range))
        cat("<tr>\n", rows[i], "</tr>\n", file = outfile, sep = "")
    
    cat("</table>", paste("<p>", length(range), " Genes</p>", sep = ""), 
        "</body>", "</html>", file = outfile, sep = "\n")
    close(outfile)
    
    if (open)
        browseURL(filename)
})

setMethod("saveText", "aafTable", function(object, filename, 
                                           header = TRUE,
                                           colnames = names(object@table), 
                                           range = 1:dim(object)[1],
                                           widget = FALSE) {

    colnames <- intersect(colnames, names(object@table))
    if (widget)
        colnames <- selectorWidget(aaf.handler(), colnames, ordernsel = TRUE,
                                   title = "Select Columns to Save")
    
    if (is.character(range))
        range <- match(range, probeids(object))
    
    rows <- vector("character", length(range))
    sep <- ""
    
    head <- paste(colnames, collapse = "\t")
    for(col in colnames) {
        rows <- paste(rows, getText(object@table[[col]][range]), sep = sep)
        sep <- "\t"
    }
    
    outfile <- file(filename, "w")
    
    if (header)
        cat(head, "\n", file = outfile, sep = "")
    cat(rows, file = outfile, sep = "\n")
    
    close(outfile)
})
