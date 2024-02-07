#' Fujita2023 longitudinal microbiome data
#'
#' A processed version of the Fujita2023 longitudinal microbiome dataset
#' The following actions have been taken to process this data:
#' * Sparsity selection: features were kept if they were <= 99% sparse
#' * CLR transform
#' * Folding into data cube
#' * Centering across the subject mode
#' * Scaling within the feature mode
#'
#' @format ## `Fujita2023`
#' A list object with three elements:
#' \describe{
#'   \item{sampleMetadata}{Dataframe with all the sample metadata, ordered the same as the rows in the data cube.}
#'   \item{taxonomy}{Taxonomic classification of the microbiota, ordered the same as the columns in the data cube.}
#'   \item{data}{Array object of the data cube}
#'   ...
#' }
#' @source <https://doi.org/10.1186/s40168-023-01474-5>
"Fujita2023"

#' Shao2019 longitudinal microbiome data
#'
#' A processed version of the Shao2019 longitudinal microbiome dataset.
#' The following actions have been taken to process this data:
#' * Sparsity selection: features were kept if they were <= 90% sparse in either birth mode group
#' * CLR transform
#' * Folding into data cube
#' * Centering across the subject mode
#' * Scaling within the feature mode
#'
#'
#' @format ## `Shao2019`
#' A list object with three elements:
#' \describe{
#'   \item{sampleMetadata}{Dataframe with all the sample metadata, ordered the same as the rows in the data cube.}
#'   \item{taxonomy}{Taxonomic classification of the microbiota, ordered the same as the columns in the data cube.}
#'   \item{data}{Array object of the data cube}
#'   ...
#' }
#' @source <https://doi.org/10.1038/s41586-019-1560-1>
"Shao2019"

#' vanderPloeg2024 longitudinal microbiome data
#'
#' A processed version of the vanderPloeg2024 longitudinal microbiome dataset.
#' The following actions have been taken to process this data:
#' * Sparsity selection: features were kept if they were <= 90% sparse in either birth mode group
#' * CLR transform
#' * Folding into data cube
#' * Centering across the subject mode
#' * Scaling within the feature mode
#'
#'
#' @format ## `vanderPloeg2024`
#' A list object with three elements:
#' \describe{
#'   \item{sampleMetadata}{Dataframe with all the sample metadata, ordered the same as the rows in the data cube.}
#'   \item{taxonomy}{Taxonomic classification of the microbiota, ordered the same as the columns in the data cube.}
#'   \item{data}{Array object of the data cube}
#'   ...
#' }
#' @source <https://doi.org/10.1186/s40168-023-01474-5>
"vanderPloeg2024"
