#' Fujita2023 longitudinal microbiome data
#'
#' The Fujita2023 longitudinal microbiome dataset as a three-dimensional array,
#' with replicates in mode 1, microbial abundances in mode 2 and time in mode 3.
#'
#' @format ## `Fujita2023`
#' A list object with three elements:
#' \describe{
#'   \item{data}{Array object of the data cube}
#'   \item{mode1}{Dataframe with all the subject metadata, ordered the same as the rows in the data cube.}
#'   \item{mode2}{Taxonomic classification of the microbiota, ordered the same as the columns in the data cube.}
#'   \item{mode3}{Dataframe with the time metadata, ordered the same as the third dimension in the array.}
#'   ...
#' }
#' @source <https://doi.org/10.1186/s40168-023-01474-5>
"Fujita2023"

#' Shao2019 longitudinal microbiome data
#'
#' The Shao2019 longitudinal microbiome dataset as a three-dimensional array,
#' with subjects in mode 1, microbial abundances in mode 2 and time in mode 3.
#' Note: only time points 4, 7, 21 and Infancy are used.
#' Note: all-zero microbial abundances have been removed to save disk space.
#'
#' @format ## `Shao2019`
#' A list object with three elements:
#' \describe{
#'   \item{data}{Array object of the data cube}
#'   \item{mode1}{Dataframe with all the subject metadata, ordered the same as the rows in the data cube.}
#'   \item{mode2}{Taxonomic classification of the microbiota, ordered the same as the columns in the data cube.}
#'   \item{mode3}{Dataframe with the time metadata, ordered the same as the third dimension in the array.}
#'   ...
#' }
#' @source <https://doi.org/10.1038/s41586-019-1560-1>
"Shao2019"

#' vanderPloeg2024 longitudinal microbiome data
#'
#' The vanderPloeg2024 longitudinal microbiome dataset as a three-dimensional array,
#' with subjects in mode 1, microbial abundances in mode 2, and time in mode 3.
#' Note: all-zero microbial abundances have been removed to save disk space.
#'
#' @format ## `vanderPloeg2024`
#' A list object with three elements:
#' \describe{
#'   \item{data}{Array object of the data cube}
#'   \item{mode1}{Dataframe with all the subject metadata, ordered the same as the rows in the data cube.}
#'   \item{mode2}{Taxonomic classification of the microbiota, ordered the same as the columns in the data cube.}
#'   \item{mode3}{Dataframe with the time metadata, ordered the same as the third dimension in the array.}
#'   ...
#' }
#' @source <https://doi.org/10.1101/2024.03.18.585469 >
"vanderPloeg2024"
