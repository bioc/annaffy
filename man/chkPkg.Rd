\name{chkPkg}
\alias{chkPkg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ A Function to Check for Installed Annotation Packages }
\description{
  This function checks for installed annotation packages. This is an
  internal function and should not be called by end-users.
}
\usage{
chkPkg(pkg)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pkg}{ A chip-level annotation package.}
}
\details{
  This function checks the installed annotation packages, including
  \code{GO} and \code{KEGG} packages.
}
\value{
  \item{pkg.ver }{ The version of the chip-level package}
  \item{go.ver }{The version of the \code{GO} package}
  \item{kg.ver}{The version of the \code{KEGG} package}
  \item{pkg.avail}{Boolean indicating if the chip-level package is
    installed}
  \item{go.avail}{Boolean indicating if the \code{GO} package is
    installed}
  \item{kg.avail}{Boolean indicating if the \code{KEGG} package is
    installed}
  \item{installable}{Boolean indicating if the library is
    user-writeable}
  \item{need.pkg}{Boolean indicating if packages need to be downloaded}
  \item{pkgs.consistent}{Boolean indicating if installed packages have
    consistent version numbers}
  \item{which.pkg}{Vector of packages needed}
}

\author{ James W. MacDonald <jmacdon@med.umich.edu>}


\keyword{ internal }% __ONLY ONE__ keyword per line
