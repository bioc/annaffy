chkPkgs <- function(pkg) {
    require("reposTools") || stop("chkPkgs requires the reposTools package")

    ## Exit early (save CPU) if we've already checked this package
    if (pkg %in% getOption("aafChkPkgs"))
      return()

    ## Hard-code version numbers. This version of chkPkgs added after
    ## feature freeze, so cannot change API by adding as arguments to function.

    aaVersMaj <- 1
    aaVersMin <- 7
  

    ## Set our min and the 'too high' versions
    minVers <- buildVersionNumber(paste(aaVersMaj, aaVersMin, 0, sep="."))
    highVers <- buildVersionNumber(paste(aaVersMaj,
                                         aaVersMin+1, 0, sep="."))

    
    if (is.installed(pkg, verbose=FALSE)) {
      curVers <- getPkgVers(pkg, verbose=FALSE)
      ok <- FALSE

      ## Check version numbers of installed package.
      for (j in seq(along=curVers)) {
        if (!is.null(curVers[[j]])){
          if ((curVers[[j]] >= minVers)&&(curVers[[j]] < highVers)) {
            ok <- TRUE
            ## If there is more than one library and the correct package
            ## is not in the first library, we have to re-order .libPaths()
            ## so we can load the correct package. With a versioned install
            ## or a lib.loc argument to require() we would not need this ugly hack.
            if (j > 1){
              oldlib <- .libPaths()
              .libPaths(c(.libPaths()[j], .libPaths()[-j]))
              require(pkg, character.only = TRUE)
              .libPaths(oldlib)
            } 
            break
          }
        }
      }
      
      if(!ok) {
        print(paste("You have package", pkg,
                    "but the incorrect version"))
      }
      else
        return()
    }
    else {
      print(paste("You are missing", pkg,
                  "looking to see if it is available."))
    }
    repList <- getReposList(list(getReposEntry("BIOCData"),
                                 getReposEntry("BIOCDevData")))
    
    pInfoList <- repPkgInfoList(repList, pkg)
    ## Remove any entries that are NULL
    pInfoList <- pInfoList[!sapply(pInfoList, is.null)]
    ## Iterate through all versions on BioC
    for (k in seq(along = pInfoList)) {
      pkgVersions <- sapply(pInfoList[[k]], function(x)
                            pkgVersion(x))
      ## Should only be one pkgVersion here, but just in case
      for (l in seq(along=pkgVersions)) {
        if ((pkgVersions[[l]] >= minVers)&&
              (pkgVersions[[l]] < highVers)) {
          ans <- userQuery(paste("Package", pkg, "version",
                                 pkgVersions[[l]], "is available",
                                 "for download, would you like to",
                                 "install?"))
          if ((ans == "yes")||(ans == "y")){
            ## added because install.packages2 chokes if the same
            ## version is in devel as well as release repositories
            if(k == 1) develOK <- FALSE else develOK <- TRUE
            install.packages2(sapply(pInfoList[[k]], pkgName),
                              versions=sapply(pInfoList[[k]],
                                function(x) as.character(pkgVersion(x))),
                              develOK=develOK)
          }
          break
        }
      }
      ## If both release and devel versions are acceptable, install
      ## the release version and quit.
      if(is.installed(pkg, vers = sapply(pInfoList[[k]],
                             function(x) as.character(pkgVersion(x)))))
        break
    }
    ## Record that we've already checked this package
    options(aafChkPkgs = c(getOption("aafChkPkgs"), pkg))
  }
