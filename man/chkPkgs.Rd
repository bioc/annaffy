\name{chkPkgs}
\alias{chkPkgs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ A Function to Check for and Install Missing Annotation Packages }
\description{
  This is a wrapper function that calls all the necessary functions to
  detect missing annotation packages, ensure all versions are compatible
  with the current version of annaffy,and download required
  packages. This is an internal function and should not be called by the
  end user.
}
\usage{
chkPkgs(pkg)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pkg}{ The chip-level annotation package}
}
\details{
  This function checks for the correct chip-level package, and if it is
  not installed will download and install. In the case that there are
  two versions of the same package installed, the function will return
  the library location of the package with the correct version.
}
\value{
  This function doesn't return anything; it is only called for its side
  effect of loading or installing a chip-level annotation package.
}

\author{ James W. MacDonald <jmacdon@med.umich.edu> and Jeff Gentry <jgentry@jimmy.harvard.edu>}

\keyword{internal}
