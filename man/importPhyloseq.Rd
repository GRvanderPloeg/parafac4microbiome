% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/importPhyloseq.R
\name{importPhyloseq}
\alias{importPhyloseq}
\title{Import Phyloseq object for PARAFAC modelling}
\usage{
importPhyloseq(phyloseqObject, subjectIDs, thirdMode)
}
\arguments{
\item{phyloseqObject}{Phyloseq object containing at least an otu table and sample data, preferably also taxonomic information.}

\item{subjectIDs}{Column name in sam_data corresponding to the subject IDs.}

\item{thirdMode}{Column name in sam_data corresponding to the study design aspect to put in the third mode of the data cube.}
}
\value{
List object containing:
\itemize{
\item 'data': data cube
\item 'mode1': metadata of the subject mode
\item 'mode2': taxonomy information
\item 'mode3': metadata of the third mode
}
}
\description{
Import Phyloseq object for PARAFAC modelling
}
\examples{
\dontshow{if (rlang::is_installed("phyloseq")) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
library(phyloseq)
data(GlobalPatterns)
GP = GlobalPatterns

# Add custom subject IDs to the sample data to make this example work
alteredSampleData = sample_data(GP)
alteredSampleData$subjectID = c(1,2,3,1,2,1,2,3,1,2,1,2,1,2,3,1,2,3,1,2,3,4,5,1,2,3)
df = phyloseq(otu_table(GP), tax_table(GP), alteredSampleData)

# Make a data cube with SampleType (soil, feces, etc.) as the third mode.
result = importPhyloseq(df, subjectIDs = "subjectID", thirdMode="SampleType")
\dontshow{\}) # examplesIf}
}
