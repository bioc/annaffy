\name{chkPkgVer}
\alias{chkPkgVer}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ A Function to Check for Annotation Package Versions }
\description{
  This function checks the version of annotation packages to ensure that
  they are consistent with the current version of annaffy. This is an
  internal function and should not be called by end-users.
}
\usage{
chkPkgVer(input)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input}{ An annotation package.}
}
\details{
  This function checks the installed annotation packages, including
  \code{GO} and \code{KEGG} packages.
}
\value{
  \item{test}{Boolean indicating if the package is compatible with the
    current version of annaffy.}
 }

\author{ James W. MacDonald <jmacdon@med.umich.edu>}


\keyword{ internal }% __ONLY ONE__ keyword per line
