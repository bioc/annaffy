\name{lookPkg}
\alias{lookPkg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ A Function to Look up Packages on BioConductor Website}
\description{
  This function looks up annotation packages on BioC and returns
  information about what is available. This is an internal function and
  should not be called by the end user.
}
\usage{
lookPkg(pkg, verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pkg}{ A \code{list} of packages to look up.}
  \item{verbose}{ Boolean indicating if some messages should be printed.}
}

\value{
 
  \item{release }{Boolean indicating if release versions are available.}
  \item{devel }{Boolean indicating if development versions are
    available.}
  \item{repList}{A \code{\link[reposTools]{repList}}}
  \item{pkg}{A list of package names}
  \item{type}{The type of package to download, \code{Win32} for windows
    and \code{Source} for Unix or MacOS.}
  \item{dev.ver}{The version number of the devel packages.}
  \item{rel.ver}{The version number of the release packages.}
}

\author{ James W. MacDonald <jmacdon@med.umich.edu>}

\keyword{ internal }% __ONLY ONE__ keyword per line
