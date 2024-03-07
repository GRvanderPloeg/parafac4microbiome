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

reinflateBlock = function(loadingVectors){

  if(methods::is(loadingVectors, "parafac")){
    model = loadingVectors
    loadingVectors = list()
    numComponents = ncol(model$A)
    numModes = length(model$const)

    loadingVectors[[1]] = model$A
    loadingVectors[[2]] = model$B
    loadingVectors[[3]] = model$C
    if(numModes == 4){
      loadingVectors[[4]] = model$D
    }
  }
  stopifnot(class(loadingVectors) == "list")
  stopifnot(length(unique(unlist(lapply(loadingVectors, ncol)))) == 1) # number of components should be equal in all modes

  numComponents = unique(unlist(lapply(loadingVectors, ncol)))
  dimX = unlist(lapply(loadingVectors, nrow))

  M = array(0L, dimX)

  # Two-way
  if(length(loadingVectors) == 2){
    for(i in 1:numComponents){
      A = matrix(loadingVectors[[1]][,i])
      B = matrix(loadingVectors[[2]][,i])
      M = M + array(tcrossprod(A, B), dimX)
    }
  }
  # Three-way
  else if(length(loadingVectors) == 3){
    for(i in 1:numComponents){
      A = matrix(loadingVectors[[1]][,i])
      B = matrix(loadingVectors[[2]][,i])
      C = matrix(loadingVectors[[3]][,i])
      M = M + array(tcrossprod(A, multiway::krprod(C, B)), dimX)
    }
  }
  # Four-way
  else if(length(loadingVectors) == 4){
    for(i in 1:numComponents){
      A = matrix(loadingVectors[[1]][,i])
      B = matrix(loadingVectors[[2]][,i])
      C = matrix(loadingVectors[[3]][,i])
      D = matrix(loadingVectors[[4]][,i])
      M = M + array(tcrossprod(A, multiway::krprod(multiway::krprod(A,C), C)), dimX)
    }
  }
  else{
    stop("reinflateBlock cannot deal with 5-way or higher order arrays.")
  }

  return(M)
}
