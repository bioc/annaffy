# Automatic package download code contributed by James MacDonald

"chkPkgs" <-
  function(pkg){
    
  # This function is a wrapper to find and install necessary annotation packages.
  # The only variable passed to the function is the name of a chip-level package,
  # although both the GO and KEGG packages will also be checked and downloaded
  # if necessary.
    
  # Modified 7-2-2004: Removed package consistency checks, replaced with package
  # version checking. This is to ensure that we don't download packages that will
  # break annaffy. For the current version, this means 1.6.x series packages.
  
  # Exit early (save CPU) if we've already checked this package
  if (pkg %in% getOption("aafChkPkgs"))
      return()
  
  # Check installed packages
  inst <- chkPkg(pkg)
  

  noinst.message <- paste("You have chosen not to install anything.\n annaffy will",
                  "now abort.\n")
  
  if(length(inst$which.pkg)){
    install <- TRUE
    tmp <- lookPkg(inst$which.pkg, verbose = TRUE)
  

  # User writeable library exists

    if(inst$installable & !tmp$fail){
      if(length(inst$which.pkg) == 1)
        inst.message <- c("Would you like to install the above annotation package?\n")
      if(length(inst$which.pkg) > 1)
        inst.message <- c("Would you like to install the above annotation packages?\n")
      installPacks(tmp, inst.message, noinst.message)
    }
    if(!inst$installable){
      stop(paste("You need to install the following annotation package(s):\n\n",
                 inst$which.pkg,"\n\nHowever, you do not have a user-writeable library.\n",
                 "Please add a path to which you have write access using .libPaths(),\n",
                 "and try again."), call. = FALSE)
    }
    if(tmp$fail){
      stop(paste("You need to install the following annotation package(s):\n\n",
                 inst$which.pkg, "\n\nHowever, it seems they are not available on",
                 "the bioconductor website.\n You might try looking on the website to",
                 "make sure, or ask at bioconductor@stat.math.ethz.ch."))
    }
  }
  # Record that we've already checked this package
  options(aafChkPkgs = c(getOption("aafChkPkgs"), pkg))
}


"chkPkg" <-
function(pkg){

  # A function to check what annotation packages are already installed
  # this function checks for the packages, and also checks to see that
  # the version numbers are compatible with the current version of annaffy.

 
  # Check for user-writeable libraries
  libs.avail <- file.access(.libPaths(), mode = 2) == 0

  # If the first library isn't user installable, then this may be a
  # pointless exercise
  installable <- libs.avail[1]

  pkg.ver <- vector()
  pkg.avail <- vector()
  go.ver <- vector()
  go.avail <- vector()
  kg.ver <- vector()
  kg.avail <- vector()
  pg <- c(pkg, "GO","KEGG")
  
  for(i in seq(along = libs.avail)){
    pkg.avail[i] <- file.exists(paste(.libPaths()[i], "/", pkg, sep=""))
    go.avail[i] <- file.exists(paste(.libPaths()[i], "/GO", sep=""))
    kg.avail[i] <- file.exists(paste(.libPaths()[i], "/KEGG", sep=""))
  }
  for(i in seq(along = libs.avail)){
    if(pkg.avail[i])
      pkg.ver[i] <- packageDescription(pkg, lib.loc = .libPaths()[i], field = "Version")
    else
      pkg.ver[i] <- NA
    if(go.avail[i])
      go.ver[i] <- packageDescription("GO", lib.loc = .libPaths()[i], field = "Version")
    else
      go.ver[i] <- NA
    if(kg.avail[i])
       kg.ver[i] <- packageDescription("KEGG", lib.loc = .libPaths()[i] ,field = "Version")
    else
      kg.ver[i] <- NA
  }

  # Check to see how many packages are installed
  # Check that installed packages have compatible versions
  # Here I rely on the fact that R loads the package from the
  # first library in which it finds the package, so duplicated
  # packages in other libraries don't matter.

  # No packages installed
  if(!any(pkg.avail, go.avail, kg.avail)){
    which.pkg <- pg
  }

  # One package installed
  installed <- which(c(any(pkg.avail), any(go.avail), any(kg.avail)))
  if(length(installed) == 1){
    if(installed == 1)
      compatible <- chkPkgVer(pkg.ver[pkg.avail][1])
    if(installed == 2)
      compatible <- chkPkgVer(go.ver[go.avail][1])
    if(installed == 3)
      compatible <- chkPkgVer(kg.ver[kg.avail][1])
    installed <- installed[compatible]
    if(length(installed) == 0)
      which.pkg <- pg
    else
      which.pkg <- pg[-indstalled]
  }
    

  # Two packages installed
  if(length(installed) == 2){
    if(sum(installed) == 3){
      pkg.compatible <- chkPkgVer(pkg.ver[pkg.avail][1])
      go.compatible <- chkPkgVer(go.ver[go.avail][1])
      installed <- installed[c(pkg.compatible, go.compatible)]
    }
    if(sum(installed) == 4){
      pkg.compatible <- chkPkgVer(pkg.ver[pkg.avail][1])
      kg.compatible <- chkPkgVer( kg.ver[kg.avail][1])
      installed <- installed[c(pkg.compatible, kg.compatible)]
    }
    if(sum(installed) == 5){
      go.compatible <- chkPkgVer(go.ver[go.avail][1])
      kg.compatible <- chkPkgVer(kg.ver[kg.avail][1])
      installed <- installed[c(go.compatible, kg.compatible)]
    }
    if(length(installed) == 0)
      which.pkg <- pg
    else
      which.pkg <- pg[-installed]
  }

  # Three packages installed
  
  if(sum(any(pkg.avail), any(go.avail), any(kg.avail)) == 3){
    pkg.compatible <- chkPkgVer(pkg.ver[pkg.avail][1])
    go.compatible <- chkPkgVer(go.ver[go.avail][1])
    kg.compatible <- chkPkgVer(kg.ver[kg.avail][1])
    installed <- installed[c(pkg.compatible, go.compatible, kg.compatible)]
    if(length(installed) == 3){
      which.pkg <- character()
    }else{
      if(length(installed) == 0)
        which.pkg <- pg
      else
        which.pkg <- pg[-installed]
    }
  }
 
  # annaffy shouldn't be in the list
  which.pkg <- which.pkg[which.pkg != "annaffy"]
 
  return(list(installable = installable, which.pkg = which.pkg))
}


 # A function to check that packages are 1.6.x. This will have to be incremented
 # as compatibility changes. Perhaps set a flag for each annaffy version that
 # indicates the compatible annotation packages?

"chkPkgVer" <-
  function(input){
    if(any(is.na(input)))
      return(FALSE)
    else{
      ver <- strsplit(input, split = "\\.")
      tmp <- matrix(unlist(ver, use.names = FALSE), nc = 3, byrow = TRUE)
      test <- all(tmp[,1] == 1, tmp[,2] == 6)
     return(test)
    }
  }



 "lookPkg" <-
  function(pkg, verbose=FALSE){

  # A function to look for available packages on the BioC website.
  # Much of this comes straight out of reposTools, so any changes
  # there may adversely effect the functionality.

  # Make an intelligible warning to the end user about what
  # packages are missing.
  
    if(length(pkg)==1){
      if(verbose)
        cat("You either do not have", unlist(pkg), "installed\n",
            "or you have an incompatible version.\n",
            "Looking to see if it is available for download\n\n")
    }else{
      n <- length(pkg)
      pkg.names <- vector()
      for(i in 1:(n-1)){
        pkg.names <- paste(pkg.names, pkg[[i]], "or", sep=" ")
      }
      pkg.names <- paste(pkg.names, pkg[[n]], sep=" ")
      if(verbose)
        
        cat("You do not have", pkg.names, "installed\n",
            "or you have incompatible versions.\n",
            "Looking to see if they are available for download\n\n")
    }

  # Get package version numbers from BioC
    
    options(warn = -1)
    require(reposTools, quietly = TRUE)
    options(warn = 0)
    repList <- buildRepList(NULL, TRUE, FALSE)
    opt <- getOption("BioC")
    type <- opt$reposEntry$type
    avail <- buildPkgListing(repList, pkg, type, develOK = TRUE)
    out <- vector("list", length(pkg))
    options(show.error.messages = FALSE)
    for(i in seq(along=pkg)){
      ver.rel <- try(as.character(pkgList(avail)[[i]][[1]][[1]]))
      if(inherits(ver.rel, "try-error")) ver.rel <- NA
    ver.dev <- try(as.character(pkgList(avail)[[i]][[2]][[1]]))
      if(inherits(ver.dev, "try-error")) ver.dev <- NA
      out[[i]] <- list( ver.rel = ver.rel,
                       ver.dev = ver.dev)
    }
    options(show.error.messages = TRUE)


    # Check to determine which BioC packages are consistent with
    # the current version of annaffy, and set download flag accordingly
    # also return fail if any BioC packages return NA

    rel.ver <- vector()
    dev.ver <- vector()
    for(i in seq(along=pkg)){
      rel.ver[i] <- out[[i]]$ver.rel
      dev.ver[i] <- out[[i]]$ver.dev
    }
    rel.test <- chkPkgVer(rel.ver)
    dev.test <- chkPkgVer(dev.ver)

    if(rel.test)
      return(list(develOK = FALSE, fail = FALSE, repList = repList,
                  type = type, pkg = pkg))
    
    if(dev.test)
      return(list(develOK = TRUE, fail = FALSE, repList = repList,
                  type = type, pkg = pkg))
    
    if(!rel.test & !dev.test)
      return(list(fail = TRUE))
  }

"installPacks" <-
  function(object, message, noinstall){

  # A function that installs packages using functionality
  # from reposTools.
  
    cat(message)
    choice <- menu(c("Install annotation packages",
                     "Abort/Don't install anything"))
      
    if(choice == 2)
      stop(noinstall, call. = FALSE)
  
  
    #Only give the choice of user-installable libraries
    use.libs <- .libPaths()[file.access(.libPaths(), mode=2) == 0]
    if(length(use.libs) == 1){
      lib <- use.libs
    }else{
      tmp <- menu(use.libs, title=c("Which library should be",
                  "used for the installation?"))
      lib <- use.libs[tmp]
    }

  # This comes straight out of the end of install.packages2() in reposTools
  # Any changes that Jeff Gentry makes there may adversely impact the functionality
  
    syncLocalLibList(lib)
    pStatList <- new("pkgStatusList", statusList = list())
    pkgList <- buildPkgListing(repList = object$repList, object$pkg, object$type, object$develOK)
    statusList(pStatList) <- baseFileSelect(pkgList, lib, object$type, getNewest = TRUE,
                                            versForce = TRUE, force = FALSE,
                                            searchOptions = FALSE,
                                            getAllDeps = FALSE, method = "auto",
                                            curRepList = object$repList)
    return(pStatList)
  }
