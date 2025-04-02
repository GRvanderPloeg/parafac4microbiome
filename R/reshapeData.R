#' Reorganize longitudinal microbiome into a data cube ready for PARAFAC modelling.
#'
#' @param Xlong Longitudinal microbiome count data in matrix (long) format.
#' @param subjectMetadata Vector containing the subjects corresponding to the measurements in Xlong.
#' @param featureMetadata Taxonomic classification of the microbiota, ordered the same as the columns in Xlong.
#' @param timepointMetadata Vector containing the time points corresponding to the measurements in Xlong.
#' @param timepointOrder Vector containing the required order of the timepoints in timepointMetadata (default: sort(unique(timepointMetadata)) ).
#'
#' @return #' A list object containing \describe{
#'   \item{data}{Array object of the data cube}
#'   \item{mode1}{Dataframe with the subjects, ordered the same as the rows in the data cube.}
#'   \item{mode2}{Taxonomic classification of the microbiota, ordered the same as the columns in the data cube.}
#'   \item{mode3}{Dataframe with the time metadata, ordered the same as the third dimension in the data cube.}
#'   ...
#' }
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' Xlong = array(rnorm(108*5*10), c(108*5, 10))
#' subjects = rep(1:108, 5)
#' features = rep(1:10)
#' timepoints = rep(1:5, each=108)
#'
#' dataset = reshapeData(Xlong, subjects, features, timepoints)
reshapeData = function(Xlong, subjectMetadata, featureMetadata, timepointMetadata, timepointOrder=sort(unique(timepointMetadata))){

  if(length(subjectMetadata) != nrow(Xlong)){
    stop("The length of the subject metadata is not equal to the number of rows in the count data, or it is not a vector.")
  }

  # Cast featureMetadata into a matrix if a vector is supplied
  if(!methods::is(featureMetadata, "data.frame")){
    featureMetadata = as.matrix(featureMetadata)
    colnames(featureMetadata) = "V1" # temporary name that is replaced later
  }

  if(nrow(featureMetadata) != ncol(Xlong)){
    stop("The number of rows in the feature metadata is not equal to the number of columns in the count data.")
  }

  if(length(timepointMetadata) != nrow(Xlong)){
    stop("The length of the time point metadata is not equal to the number of rows in the count data, or it is not a vector.")
  }

  # Homogenize character-based vs numeric-based subject metadata
  if(is.numeric(subjectMetadata)){
    subjectMetadata = as.character(subjectMetadata)
  }

  I = length(unique(subjectMetadata))
  J = ncol(Xlong)
  K = length(unique(timepointMetadata))
  X = array(0L, c(I,J,K))

  # Make subjectMetadata into a tibble to do proper set operations
  subjectInfo = unique(subjectMetadata) %>% dplyr::as_tibble()
  colnames(subjectInfo) = c("subjectMetadata", "index")
  subjectInfo = subjectInfo %>% dplyr::arrange(subjectMetadata)
  subjectInfo = subjectInfo %>% dplyr::mutate(index=1:nrow(subjectInfo))

  # Make featureMetadata into a tibble to get the appropriate mode3 result
  featureInfo = featureMetadata %>% dplyr::as_tibble()
  featureInfo = featureInfo %>% dplyr::mutate(index=1:nrow(featureInfo))

  # Make timepointMetadata into a tibble to get the appropriate mode3 result
  timepointInfo = timepointOrder %>% dplyr::as_tibble()
  timepointInfo = timepointInfo %>% dplyr::mutate(index=1:nrow(timepointInfo))
  colnames(timepointInfo) = c("timepointMetadata", "index")

  # Merge counts and metadata
  df = cbind(Xlong, subjectMetadata, timepointMetadata)
  colnames(df) = c(paste0("V", 1:ncol(Xlong)), "subjectMetadata", "timepointMetadata")
  df = df %>% dplyr::as_tibble()

  for(k in 1:K){
    X[,,k] = df %>%
      dplyr::filter(timepointMetadata == timepointOrder[k]) %>%
      dplyr::right_join(subjectInfo %>% dplyr::select(subjectMetadata) %>% unique(), by="subjectMetadata") %>%
      dplyr::arrange(subjectMetadata) %>%
      dplyr::select(-dplyr::all_of(c("subjectMetadata", "timepointMetadata"))) %>%
      as.matrix()
  }

  class(X) <- "numeric"

  result = list("data"=X, "mode1"=subjectInfo, "mode2"=featureInfo, "mode3"=timepointInfo)
  return(result)
}
