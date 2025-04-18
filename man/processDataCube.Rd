% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/processDataCube.R
\name{processDataCube}
\alias{processDataCube}
\title{Process a multi-way array of count data.}
\usage{
processDataCube(
  dataset,
  sparsityThreshold = 1,
  considerGroups = FALSE,
  groupVariable = "",
  CLR = TRUE,
  centerMode = 0,
  scaleMode = 0
)
}
\arguments{
\item{dataset}{A longitudinal microbiome dataset, formatted as follows:
\describe{
\item{data}{Array object of the data cube filled with counts}
\item{mode1}{Dataframe with all the subject metadata, ordered the same as the rows in the data cube.}
\item{mode2}{Taxonomic classification of the microbiota, ordered the same as the columns in the data cube.}
\item{mode3}{Dataframe with the time metadata, ordered the same as the third dimension in the array.}
}
See \link{Fujita2023}, \link{Shao2019} or \link{vanderPloeg2024} for more information.}

\item{sparsityThreshold}{Maximum sparsity for a feature to be selected (default=1, i.e. do not select features).}

\item{considerGroups}{Consider groups when calculating sparsity (default=FALSE).}

\item{groupVariable}{Column name in dataset$mode1 that should be used to consider groups (default="").}

\item{CLR}{Perform a centered log-ratio transformation of the count data (default=TRUE).}

\item{centerMode}{Mode to center across: 1=subjects,2=features,3=time (default 0, i.e. do not center). See \code{\link[=multiwayCenter]{multiwayCenter()}} for more information.}

\item{scaleMode}{Mode to scale within: 1=subjects,2=features,3=time (default 0, i.e. do not scale). See \code{\link[=multiwayScale]{multiwayScale()}} for more information.}
}
\value{
CLRed, centered and scaled cube
}
\description{
Process a multi-way array of count data.
}
\examples{
processedCube = processDataCube(Fujita2023)
}
