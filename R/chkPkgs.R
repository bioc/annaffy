# Automatic package download code contributed by James MacDonald

"chkPkgs" <-
function(pkg){
  
  # This function is a wrapper to find and install necessary annotation packages.
  # The only variable passed to the function is the name of a chip-level package,
  # although both the GO and KEGG packages will also be checked and downloaded
  # if necessary.
  
  # Exit early (save CPU) if we've already checked this package
  if (pkg %in% getOption("aafChkPkgs"))
      return()
  
  # Check installed packages
  inst <- chkPkg(pkg)
  

  noinst.message <- paste("You have chosen not to install anything.\n annaffy will",
                  "now abort.\n")
  install <- TRUE

  # Only one library
  if(length(inst$pkg.ver) == 1){
    instpkgs <- c(inst$pkg.avail, inst$go.avail, inst$kg.avail)
    if(any(is.na(instpkgs)))
      instpkgs[which(is.na(instpkgs))] <- FALSE
    instpkver <- c(inst$pkg.ver, inst$go.ver, inst$kg.ver)[instpkgs]
    num.avail <- sum(instpkgs)
  }
  # If more than one library, get version and availability for the
  # packages that will be loaded by default
  if(length(inst$pkg.ver) > 1){
    index <- c(which(!is.na(inst$pkg.ver))[1],
               which(!is.na(inst$go.ver))[1],
               which(!is.na(inst$kg.ver))[1])
    instpkgs <- c(inst$pkg.avail[index][1], inst$go.avail[index][2], inst$kg.avail[index][3])
    if(any(is.na(instpkgs)))
      instpkgs[which(is.na(instpkgs))] <- FALSE
    instpkver <- c(inst$pkg.ver[index][1], inst$go.ver[index][2], inst$kg.ver[index][3])[instpkgs]
    num.avail <- sum(instpkgs)
  } 

  # User writeable library exists

  if(inst$installable){
    
   
    #  No packages yet installed
    if(inst$need.pkg & num.avail == 0){
      pks2get <- list(pkg, "GO", "KEGG")
      inst.message <- paste("You need to install", pkg,", GO, and KEGG annotation",
                            "packages.\n Please make a choice from this list:\n")
      tmp <- lookPkg(pks2get)
    }
   
    # All packages already installed
    if(num.avail == 3){

    #Check that version of installed packages match

      if(inst$pkgs.consistent)
        install <- FALSE
      if(!inst$pkgs.consistent){
        pks2get <- list(pkg, "GO","KEGG")
        inst.message <- paste("The existing annotation packages have mis-matched",
                              "versions.\n This is likely to cause problems for annaffy.\n",
                              "You should install all packages.")
        tmp <- lookPkg(pks2get)
       
      }
    }
  
    
   
    # Some packages installed
    if(num.avail < 3 & num.avail > 0){

     
     # Check that versions of installed packages match
     # If installed versions are mis-matched, re-install
    
      if(!inst$pkgs.consistent){
        pks2get <- list(pkg, "GO","KEGG")
        inst.message <- paste("The existing annotation packages have mis-matched",
                              "versions.\n This is likely to cause problems for annaffy.\n",
                              "You should install all packages.")
        tmp <- lookPkg(pks2get)
      }else{
        pks2get <- list(pkg, "GO", "KEGG")[!instpkgs]
     
       # If some packages are already installed, find packages on BioC with the same
       # version number and suggest that they should be installed.

        tmp <- lookPkg(pks2get, verbose = TRUE)
        which.ver <- match(instpkver[1], c(tmp$rel.ver, tmp$dev.ver))

       # pkg doesn't match anything on BioC

        if(is.na(which.ver) & is.na(tmp$rel.ver) & is.na(tmp$dev.ver)){
          choice <- menu(c("Open a browser",
                           "Thanks, I'll do it myself"),
                         title = paste("The package(s)(",pks2get,") cannot be found. This may be due\n",
                           "to an incorrect package name, the BioC website may be down,\n",
                           "or your version of reposTools may be outdated. For now, you can\n",
                           "try to get the necessary packages by hand.", sep=""))
         
          if(choice == 1)
            browseURL("http://www.bioconductor.org/data/metaData.html")
          stop("Please re-run the previous commands\n when you have the necessary packages installed.\n")
        }
         # Old packages installed
        if(is.na(which.ver) & tmp$release & tmp$devel){
          choice <- menu(c("Install all packages",
                           "Abort/don't install"),
                         title = c("The annotation packages that are already installed are outdated.\n",
                           "You should re-install all packages:\n"))
          if(choice == 1)
            tmp <- lookPkg(list(pkg, "GO", "KEGG"))
          if(choice == 2)
            stop(noinst.message, call. = FALSE)
          
        }else{
        
          
          # Release packages installed, release avail on BioC
          if(which.ver == 1 & tmp$release)
            inst.message <- paste("You already have some release version annotation packages",
                                  "installed.\n You should therefore install the release packages.\n")
          
          # Devel packages installed, devel on BioC
          if(which.ver == 2 & tmp$devel & !is.na(tmp$dev.ver))
            inst.message <- paste("You already have some development version annotation packages",
                                  "installed.\n You should therefore install the development packages.\n")
         
          # Release packages installed, release mismatch/availability problems on BioC
          if(which.ver == 1 & !tmp$release){
            choice <- menu(c("Install anyway",
                             "Abort/don't install"),
                           title = c("You already have some release version annotation packages",
                             "installed.\n However, there is either a version problem or missing",
                             "release\n packages on Bioconductor. You might want to contact",
                             "Bioconductor\n <bioconductor@stat.math.ethz.ch> to see if there is a problem.\n"))
            if(choice == 2)
              stop(noinst.message, call. = FALSE)
          }
          # Devel packages installed, mismatch/availability problems on BioC
          if(which.ver == 2 & !tmp$devel){
            choice <- menu(c("Install anyway",
                             "Abort/don't install"),
                           title = c("You already have some development version annotation packages",
                             "installed.\n However, there is either a version problem or missing",
                             "development\n packages on Bioconductor. You might want to contact",
                             "Bioconductor\n <bioconductor@stat.math.ethz.ch> to see if there is a problem.\n"))
            if(choice == 2)
              stop(noinst.message, call. = FALSE)
          }
          if(which.ver == 2 & tmp$devel & is.na(tmp$dev.ver)){
            choice <- menu(c("Re-install release versions",
                             "Abort/don't install"),
                           title = c("You already have some development version annotation packages",
                             "installed.\n However, the package you want to install from Bioconductor",
                             "is only available\n in a release version. If you want to proceed, you",
                             "should probably\n re-install the release versions.\n"))
          }
        }
      }
    }
    if(install)
      installPacks(tmp, inst.message, noinst.message)
  }else{
    if(num.avail < 3)
      stop(paste("You need to install these packages:", c(pkg, "GO","KEGG")[!instpkgs],"\n",
                 "However, there does not appear to be a user-writeable library!\n",
                 "If you have a library with write access, either run .libPaths() to\n",
                 "add it to your library path, or (better yet), add the\n",
                 ".libPaths() command to your .Rprofile file, and try again."), call. = FALSE)
    if(!inst$pkgs.consistent)
      stop(paste("You need to install these packages:", pkg,", GO and KEGG\n",
                 "However, there does not appear to be a user-writeable library!\n",
                 "If you have a library with write access, either run .libPaths() to\n",
                 "add it to your library path, or (better yet), add the\n",
                 ".libPaths() command to your .Rprofile file, and try again."), call. = FALSE)
  }
  
  # Record that we've already checked this package
  options(aafChkPkgs = c(getOption("aafChkPkgs"), pkg))
}


"chkPkg" <-
function(pkg){

  # A function to check what annotation packages are already installed
  # this function checks for the packages, and also checks to see that
  # the version numbers are consistent.

  # Check for user-writeable libraries
  libs.avail <- chkLib(.libPaths())

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
  # Check that installed packages have consistent versions
  # Here I rely on the fact that R loads the package from the
  # first library in which it finds the package, so duplicated
  # packages in other libraries don't matter.

  # No packages installed
  if(!any(pkg.avail, go.avail, kg.avail)){
    need.pkg <- pkgs.consistent <- TRUE
    which.pkg <- pg
  }

  # One package installed
  installed <- which(c(any(pkg.avail), any(go.avail), any(kg.avail)))
  if(length(installed) == 1){
    need.pkg <- pkgs.consistent <- TRUE
    which.pkg <- pg[-installed]
  }
    

  # Two packages installed
  if(length(installed) == 2){
    need.pkg <- TRUE
    if(sum(installed) == 3)
      pkgs.consistent <- pkg.ver[pkg.avail][1] == go.ver[go.avail][1]
    if(sum(installed) == 4)
      pkgs.consistent <- pkg.ver[pkg.avail][1] == kg.ver[kg.avail][1]
    if(sum(installed) == 5)
      pkgs.consistent <- go.ver[go.avail][1] == kg.ver[kg.avail][1]
    which.pkg <- pg[-installed]
  }

  # Three packages installed
  
  if(sum(any(pkg.avail), any(go.avail), any(kg.avail)) == 3){
    pkgs.consistent <- (pkg.ver[pkg.avail][1] == go.ver[go.avail][1] &
                        pkg.ver[pkg.avail][1] == kg.ver[kg.avail][1])
    if(pkgs.consistent){
      need.pkg <- FALSE
      which.pkg <- NA
    }else{
      need.pkg <- TRUE
      which.pkg <- pg
    }
  }
 
  return(list(pkg.ver=pkg.ver, go.ver = go.ver, kg.ver = kg.ver,
              pkg.avail = pkg.avail, go.avail = go.avail, kg.avail = kg.avail,
              installable = installable, need.pkg = need.pkg,
              pkgs.consistent = pkgs.consistent, which.pkg = which.pkg))
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
      cat("You do not have", unlist(pkg), "installed\n",
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
          "Looking to see if they are available for download\n")
  }

  # Get package version numbers from BioC 
  
  require(reposTools, quietly = TRUE, warn.conflicts = FALSE)
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

  # Check to make sure BioC packages have consistent versions
  # This assumes <= 3 packages, which AFAIK is all we need.

  rel.ver <- vector()
  dev.ver <- vector()
  for(i in seq(along=pkg)){
    rel.ver[i] <- out[[i]]$ver.rel
    dev.ver[i] <- out[[i]]$ver.dev
  }

  if(length(pkg) == 1) release <- devel <- TRUE
  if(length(pkg) == 2){
    release <- rel.ver[1] == rel.ver[2]
    devel <- dev.ver[1] == dev.ver[2]
  }
  if(length(pkg) == 3){
    release <- all(rel.ver[1] == rel.ver[2], rel.ver[1] == rel.ver[3])
    devel <- all(dev.ver[1] == dev.ver[2], dev.ver[1] == dev.ver[3])
  }
  return(list(release = release, devel = devel, repList = repList, pkg = pkg, type = type,
              dev.ver = dev.ver, rel.ver = rel.ver))
}


"installPacks" <-
function(object, message, noinstall, install = TRUE){

  # A function that installs packages using functionality
  # from reposTools.
  
  cat(message)
  choice <- menu(c("Install release annotation packages",
                   "Install development annotation packages",
                   "Abort/Don't install anything"))
  
  if(choice == 1)
    develOK <- FALSE
   
  # Check for missing devel version on BioC
  if(choice == 2 & any(!is.na(object$dev.ver)))
    develOK <- TRUE
  if(choice == 2 & any(is.na(object$dev.ver))){
    choice2 <- menu(c("Install release annotation packages",
                     "Abort/don't install anything"),
                   title = c("Unfortunately there isn't a development package",
                     "available.\n Would you like to install the release version?"))
    if(choice2 == 1)
      develOK <- FALSE
    if(choice2 == 2)
      stop(noinstall, call. = FALSE)
  }
  if(choice == 3)
    stop(noinstall, call. = FALSE)
  
  
    #Only give the choice of user-installable libraries
  use.libs <- .libPaths()[chkLib(.libPaths())]
  if(length(use.libs) == 1){
    lib <- use.libs
  }else{
    tmp <- menu(use.libs, title="Which library should be",
                "used for the installation?")
    lib <- use.libs[tmp]
  }

  # This comes straight out of the end of install.packages2() in reposTools
  # Any changes that Jeff Gentry makes there may adversely impact the functionality
  
  syncLocalLibList(lib)
  pStatList <- new("pkgStatusList", statusList = list())
  pkgList <- buildPkgListing(repList = object$repList, object$pkg, object$type, develOK)
  statusList(pStatList) <- baseFileSelect(pkgList, lib, object$type, getNewest = TRUE,
                                          versForce = TRUE, force = FALSE,
                                          searchOptions = FALSE,
                                          getAllDeps = FALSE, method = "auto",
                                          curRepList = object$repList)
  return(pStatList)
}


"chkLib" <-
function(paths){
  
  # A small function to test for user-writeable libraries
  # by simply trying to add a directory to the .libPaths()
  # Any thing that does get written is then removed.
  
  tst <- vector()
  for(i in seq(along = paths)){
    tst[i] <- dir.create(paste(paths[i], "/tmp", sep=""))
    if(tst[i])
      unlink(paste(paths[i], "/tmp", sep=""), recursive = TRUE)
  }
  return(tst)
}
