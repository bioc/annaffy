.First.lib <- function (libname, pkgname)
{
    require(Biobase)
    require(GO)
    require(KEGG)

    path = .path.package(pkgname)
    where <- as.environment(match(paste("package:", pkgname, sep = ""),search()))
    .initAnnotation(where)
    .initTable(where)
    dataPath = file.path(path, "data")
    rdas <- list.files(path = dataPath, pattern = "*.rda")
    rdas <- gsub("(^.*)\.rda", "\\1", rdas)
    for (i in rdas)
        load(file.path(path, "data", paste(i, ".rda", sep = "")), envir = where)
    options(sigfigs=6)
}
