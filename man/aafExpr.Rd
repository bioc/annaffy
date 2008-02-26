\name{aafExpr}
\alias{aafExpr}
\title{Sample ExpressionSet used for demonstration purposes}
\description{
Contains expression values for 250 probe ids with 8 samples. Two
covariates are provided. Expression comes from the \code{hgu95av2}
chip.
}
\details{
The data is real but anonymized. 250 genes expression values were
chosen at random from an existing ExpressionSet. Another 250 probe ids
were selected at random and were assigned to the expression values.
That way, expression values do not correspond to the true probe
ids.

Post-processing was done with \code{rma()} in \code{affy} 1.2.23.
}

\keyword{datasets}
