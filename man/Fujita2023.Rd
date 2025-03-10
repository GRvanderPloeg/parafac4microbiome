% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{Fujita2023}
\alias{Fujita2023}
\title{Fujita2023 longitudinal microbiome data}
\format{
\subsection{\code{Fujita2023}}{

A list object with three elements:
\describe{
\item{data}{Array object of the data cube}
\item{mode1}{Dataframe with all the subject metadata, ordered the same as the rows in the data cube.}
\item{mode2}{Taxonomic classification of the microbiota, ordered the same as the columns in the data cube.}
\item{mode3}{Dataframe with the time metadata, ordered the same as the third dimension in the array.}
...
}
}
}
\source{
\doi{10.1186/s40168-023-01474-5}
}
\usage{
Fujita2023
}
\description{
The Fujita2023 longitudinal microbiome dataset as a three-dimensional array,
with replicates in mode 1, microbial abundances in mode 2 and time in mode 3.
}
\keyword{datasets}
