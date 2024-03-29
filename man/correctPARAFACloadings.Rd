% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{correctPARAFACloadings}
\alias{correctPARAFACloadings}
\title{Transform PARAFAC loadings to an orthonormal basis.
Note: this function only works for 3-way PARAFAC models.}
\usage{
correctPARAFACloadings(dataset, model, modeToCorrect, moreOutput = FALSE)
}
\arguments{
\item{dataset}{A longitudinal microbiome dataset, ideally processed with \code{\link[=processDataCube]{processDataCube()}}, formatted as follows:
\describe{
\item{data}{Array object of the data cube}
\item{mode1}{Dataframe with all the subject metadata, ordered the same as the rows in the data cube.}
\item{mode2}{Taxonomic classification of the microbiota, ordered the same as the columns in the data cube.}
\item{mode3}{Dataframe with the time metadata, ordered the same as the third dimension in the array.}
}}

\item{model}{Model output from \code{\link[=parafac]{parafac()}}.}

\item{modeToCorrect}{Correct the subject (1), feature (2) or time mode (3).}

\item{moreOutput}{Give orthonormal basis and transformation matrices as part of output (default FALSE).}
}
\value{
Corrected loadings of the specified mode.
}
\description{
Transform PARAFAC loadings to an orthonormal basis.
Note: this function only works for 3-way PARAFAC models.
}
\examples{
library(multiway)
library(dplyr)
library(ggplot2)
library(paramGUI)
library(pracma)
set.seed(0)

# Make PARAFAC model
processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, centerMode=1, scaleMode=2)
model = parafac(processedFujita$data, nfac=2, nstart=1, verbose=FALSE)

correctedA = correctPARAFACloadings(processedFujita, model, 1)
plot(correctedA[,1], correctedA[,2])
}
