% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotModelStability.R
\name{plotModelStability}
\alias{plotModelStability}
\title{Plot a summary of the loadings of many initialized parafac models.}
\usage{
plotModelStability(
  models,
  dataset,
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
\item{models}{Models list output from \code{\link[=parafac]{parafac()}} using output="all".}

\item{dataset}{A longitudinal microbiome dataset, ideally processed with \code{\link[=processDataCube]{processDataCube()}}, formatted as follows:
\describe{
\item{data}{Array object of the data cube}
\item{mode1}{Dataframe with all the subject metadata, ordered the same as the rows in the data cube.}
\item{mode2}{Taxonomic classification of the microbiota, ordered the same as the columns in the data cube.}
\item{mode3}{Dataframe with the time metadata, ordered the same as the third dimension in the array.}
}}

\item{colourCols}{Vector of strings stating which column names should be factorized for colours per mode.}

\item{legendTitles}{Vector of strings stating the legend title per mode.}

\item{xLabels}{Vector of strings stating the x-axis labels per mode.}

\item{legendColNums}{Vector of integers stating the desired number of columns for the legends per mode.}

\item{arrangeModes}{Vector of boolean values per mode, stating if the loadings should be arranged according to colourCols (TRUE) or not (FALSE).}

\item{continuousModes}{Vector of boolean values per mode, stating if the loadings should be plotted as a line plot (TRUE) or a bar plot (FALSE).}

\item{overallTitle}{Overall title of the plot.}
}
\value{
Plot of loadings with error bars
}
\description{
Plot a summary of the loadings of many initialized parafac models.
}
\examples{
processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, centerMode=1, scaleMode=2)
models = parafac(processedFujita$data, 2, nstart=10, output="all")
plotModelStability(models, processedFujita)
}
