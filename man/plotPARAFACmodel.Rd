% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotPARAFACmodel.R
\name{plotPARAFACmodel}
\alias{plotPARAFACmodel}
\title{Plot a PARAFAC model}
\usage{
plotPARAFACmodel(
  model,
  dataset,
  numComponents,
  colourCols = NULL,
  legendTitles = NULL,
  xLabels = NULL,
  legendColNums = NULL,
  arrangeModes = NULL,
  continuousModes = NULL,
  overallTitle = ""
)
}
\arguments{
\item{model}{Model output from \code{\link[=parafac]{parafac()}}.}

\item{dataset}{A longitudinal microbiome dataset, ideally processed with \code{\link[=processDataCube]{processDataCube()}}, formatted as follows:
\describe{
\item{data}{Array object of the data cube}
\item{mode1}{Dataframe with all the subject metadata, ordered the same as the rows in the data cube.}
\item{mode2}{Taxonomic classification of the microbiota, ordered the same as the columns in the data cube.}
\item{mode3}{Dataframe with the time metadata, ordered the same as the third dimension in the array.}
}}

\item{numComponents}{Number of PARAFAC components in the model.}

\item{colourCols}{Vector of strings stating which column names should be factorized for colours per mode.}

\item{legendTitles}{Vector of strings stating the legend title per mode.}

\item{xLabels}{Vector of strings stating the x-axis labels per mode.}

\item{legendColNums}{Vector of integers stating the desired number of columns for the legends per mode.}

\item{arrangeModes}{Vector of boolean values per mode, stating if the loadings should be arranged according to colourCols (TRUE) or not (FALSE).}

\item{continuousModes}{Vector of boolean values per mode, stating if the loadings should be plotted as a line plot (TRUE) or a bar plot (FALSE).}

\item{overallTitle}{Overall title of the plot.}
}
\value{
Plot object
}
\description{
Plot a PARAFAC model
}
\examples{
library(multiway)
library(dplyr)
library(ggplot2)
set.seed(0)

# Process the data
processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.9, centerMode=1, scaleMode=2)

# Make PARAFAC model
model = parafac(processedFujita$data, nfac=2, nstart=10, verbose=FALSE)

# Make plot
plotPARAFACmodel(model, processedFujita,
  numComponents = 2,
  colourCols = c("", "Genus", ""),
  legendTitles = c("", "Genus", ""),
  xLabels = c("Replicate", "Feature index", "Time point"),
  legendColNums = c(0,5,0),
  arrangeModes = c(FALSE, TRUE, FALSE),
  continuousModes = c(FALSE,FALSE,TRUE),
  overallTitle = "Fujita PARAFAC model")

}
