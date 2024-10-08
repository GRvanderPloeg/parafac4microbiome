% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculateVarExp.R
\name{calculateVarExp}
\alias{calculateVarExp}
\title{Calculate the variation explained by a PARAFAC model.}
\usage{
calculateVarExp(Fac, X)
}
\arguments{
\item{Fac}{Fac object output from the \code{\link[=parafac]{parafac()}} function.}

\item{X}{Input data of the PARAFAC model.}
}
\value{
The variation explained by the model, expressed as a fraction (between 0-1).
}
\description{
Calculate the variation explained by a PARAFAC model.
}
\examples{
X = Fujita2023$data
model = parafac(X, nfac=1, nstart=1, verbose=FALSE)
calculateVarExp(model$Fac, X)
}
