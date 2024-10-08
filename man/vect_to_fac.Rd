% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{vect_to_fac}
\alias{vect_to_fac}
\title{Convert vectorized output of PARAFAC to a Fac list object with all loadings per mode.}
\usage{
vect_to_fac(vect, X, sortComponents = FALSE)
}
\arguments{
\item{vect}{Vectorized output of PARAFAC modelling}

\item{X}{Input data}

\item{sortComponents}{Sort the order of the components by variation explained (default FALSE).}
}
\value{
Fac: list object with all loadings in all components per mode, ordered the same way as Z$modes.
}
\description{
Convert vectorized output of PARAFAC to a Fac list object with all loadings per mode.
}
\examples{
set.seed(123)
A = array(rnorm(108*2), c(108, 2))
B = array(rnorm(100*2), c(100, 2))
C = array(rnorm(10*2), c(10, 2))

X = reinflateTensor(A, B, C)
result = initializePARAFAC(X, 2, initialization="random", output="vect")
Fac = vect_to_fac(result, X)
}
