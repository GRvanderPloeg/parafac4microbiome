% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{fac_to_vect}
\alias{fac_to_vect}
\title{Vectorize Fac object}
\usage{
fac_to_vect(Fac)
}
\arguments{
\item{Fac}{Fac object output of \link{parafac}.}
}
\value{
Vectorized Fac object
}
\description{
Vectorize Fac object
}
\examples{
set.seed(123)
A = array(rnorm(108*2), c(108, 2))
B = array(rnorm(100*2), c(100, 2))
C = array(rnorm(10*2), c(10, 2))
Fac = list(A, B, C)
v = fac_to_vect(Fac)
}
