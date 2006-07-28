chkPkgs <- function(pkg) {
  ## Exit early (save CPU) if we've already checked this package
  if (pkg %in% getOption("aafChkPkgs"))
    return()
  
  pkgLoc <- .find.package(pkg, quiet=TRUE)
  if (length(pkgLoc) == 0) { ## pkg not installed
    print(paste("You are missing", pkg,
                "looking to see if it is available."))
    
    biocContribUrl <- sapply(Biobase:::biocReposList(), contrib.url)
    biocPkgs <- available.packages(biocContribUrl)
    if (pkg %in% biocPkgs[, "Package"]) {
      ans <- userQuery(paste("Package", pkg, "is available for",
                             "download, would you like to install?"))
      
      if (ans == "y")
        install.packages(pkg, repos=Biobase:::biocReposList(),
                         dependencies="Depends")
      
    } else {
      print(paste("Package", pkg, "was not found in the Bioconductor",
                  "repository."))
    }
  } else
  if (!is.annpkg(pkg))
    warning(paste("The", pkg, "package does not appear to contain",
                  "annotation data."))

  ## Record that we've already checked this package
  options(aafChkPkgs = c(getOption("aafChkPkgs"), pkg))
}
