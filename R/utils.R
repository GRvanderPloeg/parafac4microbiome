#' Reformat parafac model output to plottable data.
#'
#' @param model Model object (output of [multiway::parafac()])
#' @param metadataPerMode List object with metadata per mode in the cube

#' @return List object of the reformatted modes.
#' @export
#'
#' @examples
#' library(multiway)
#' library(dplyr)
#' library(ggplot2)
#' set.seed(0)
#'
#' # Make PARAFAC model
#' processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, centerMode=1, scaleMode=2)
#' model = parafac(processedFujita$data, nfac=3, nstart=100, verbose=FALSE)
#'
#' # Flipping the sign of some components to make the output plot equal to the paper.
#' model = resign(model, mode="A", absorb="C")
#' model = resign(model, mode="B", absorb="C")
#'
#' metadataPerMode = list(processedFujita$mode1, processedFujita$mode2, processedFujita$mode3)
#' convertedModel = convertModelFormat(model, metadataPerMode)
convertModelFormat = function(model, metadataPerMode=list()){
  stopifnot(class(model) == "parafac")
  stopifnot(class(metadataPerMode) == "list")

  numModes = length(model$const)
  numComponents = ncol(model$A)

  # Convert model object to list for easy iterative access
  if(numModes == 3){
    listedModel = list(model$A, model$B, model$C)
  }
  if(numModes == 4){
    listedModel = list(model$A, model$B, model$C, model$D)
  }

  # Check if the metadata per mode makes sense
  useMetadata = rep(FALSE, numModes)
  if(length(metadataPerMode) != 0){
    for(i in 1:numModes){
      loadings = listedModel[[i]]
      metadata = metadataPerMode[[i]]

      if(is.null(nrow(metadata))){
        warning(paste0("Metadata table of mode ", i, " does not match the model."))
      } else if(nrow(loadings) != nrow(metadata)){
        warning(paste0("Metadata table of mode ", i, " does not match the model."))
      } else{
        useMetadata[i] = TRUE
      }
    }
  }

  # Convert format
  result = list()
  for(i in 1:numModes){
    if(useMetadata[i] == TRUE){
      result[[i]] = cbind(listedModel[[i]], metadataPerMode[[i]]) %>% dplyr::as_tibble()
      colnames(result[[i]]) = c(paste0("Component_", 1:numComponents), colnames(metadataPerMode[[i]]))
    }
    else{
      result[[i]] = listedModel[[i]] %>% dplyr::as_tibble(.name_repair = ~ vctrs::vec_as_names(..., repair = "unique", quiet=TRUE))
      colnames(result[[i]]) = paste0("Component_", 1:numComponents)
    }
  }

  return(result)
}

checkForFlippedLoadings = function(loadingMatrix){

  numRepetitions = ncol(loadingMatrix)

  # Arbitrarily define the "correct loadings" as the median of all folds
  correctLoadings = apply(loadingMatrix, 1, function(x){stats::median(x, na.rm=TRUE)})
  evidence = rep(FALSE, numRepetitions)

  # Check every repetition
  for(j in 1:numRepetitions){
    loading = loadingMatrix[,j]
    flippedLoading = -1*loading

    diff = sum((correctLoadings-loading)^2, na.rm=TRUE)
    flippedDiff = sum((correctLoadings-flippedLoading)^2, na.rm=TRUE)

    if(flippedDiff < diff){
      evidence[j] = TRUE
    }
  }
  return(evidence)
}

repairLoadings = function(A, B, C, evidenceMatrix){
  # TODO: not robust towards 4 modes
  loadingsList = list(A, B, C)
  numRepetitions = ncol(A)
  numModes = length(loadingsList)

  repairedA = array(0L, dim=dim(A))
  repairedB = array(0L, dim=dim(B))
  repairedC = array(0L, dim=dim(C))
  repairedLoadingsList = list(repairedA, repairedB, repairedC)

  flipped = which(colSums(evidenceMatrix) >= 2) # exactly 2 modes need to be flipped to cancel out

  for(i in 1:numRepetitions){
    numFlipped = 0
    for(j in 1:numModes){
      if(i %in% flipped & evidenceMatrix[j,i] & numFlipped < 2){
        repairedLoadingsList[[j]][,i] = -1*loadingsList[[j]][,i]
        numFlipped = numFlipped + 1
      }
      else{
        repairedLoadingsList[[j]][,i] = loadingsList[[j]][,i]
      }
    }
  }

  return(repairedLoadingsList)
}
