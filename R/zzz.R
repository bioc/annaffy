.First.lib <- function (libname, pkgname)
{
    path = .path.package(pkgname)
    where <- as.environment(match(paste("package:", pkgname, sep = ""),search()))
    dataPath = file.path(path, "data")
    rdas <- list.files(path = dataPath, pattern = "\\.*.rda")
    rdas <- gsub("(^.*)\.rda", "\\1", rdas)
    for (i in rdas)
        load(file.path(path, "data", paste(i, ".rda", sep = "")), envir = where)
    options(sigfigs=6)
}
