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
  By default, these functions also check for the version and
  availability of the \code{GO} and \code{KEGG} packages.
}
\value{
  This function does not return anything. It is only called for the side
  effect of checking and getting annotation packages.
}

\author{ James W. MacDonald <jmacdon@med.umich.edu> }

\keyword{internal}
