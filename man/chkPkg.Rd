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
  \item{installable}{Boolean indicating if the library is
    user-writeable}
   \item{which.pkg}{Vector of packages needed}
}

\author{ James W. MacDonald <jmacdon@med.umich.edu>}


\keyword{ internal }% __ONLY ONE__ keyword per line
