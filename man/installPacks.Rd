\name{installPacks}
\alias{installPacks}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ A Function to Install Annotation Packages from BioConductor Website}
\description{
  This function downloads and installs annotation packages from BioC,
  with some user-interaction. This is an internal function and should
  not be called by the end-user.
}
\usage{
installPacks(object, message, noinstall)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{ A list passed from \code{chkPkgs} }
  \item{message}{ Message to post if the packages are to be installed. }
  \item{noinstall}{ Message to post if user decides not to install.}
}

\value{
 The only thing returned is a message stating if the packages were
 installed correctly or not.
}

\author{ James W. MacDonald <jmacdon@med.umich.edu>}

\keyword{ internal }% __ONLY ONE__ keyword per line
