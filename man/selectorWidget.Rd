\name{selectorWidget}
\alias{selectorWidget}
\title{Dialog to select items from a list}
\description{
  Presents the user with a dialog box to select items from a list.
}

\usage{
selectorWidget(options, selected = character(0), title = "Selector",
               ordersel = FALSE, ordernsel = FALSE, 
               height = max(10,min(20,length(options))))
}
\arguments{
  \item{options}{vector, options to be selected from}
  \item{selected}{vector, subset of options selected by default}
  \item{title}{character scalar, window title}
  \item{ordersel}{boolean, keep the selected items in order?}
  \item{ordernsel}{boolean, keep the not selected items in order?}
  \item{height}{scalar, height of the two listboxes}
}
\value{
  A character vector containing the selected items. If a vector of
  a different class was initially provided, it must be manually
  coerced back to the correct type.
}
\author{Colin A. Smith, \email{annaffy@colinsmith.org}}
\keyword{ file }
