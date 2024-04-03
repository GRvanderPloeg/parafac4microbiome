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

#' Reconstruct the data cube based on a PARAFAC model.
#'
#' Note: currently only works for three-way data.
#' @inheritParams plotPARAFACmodel
#'
#' @return Multi-way array of the reconstructed data.
#' @export
#'
#' @examples
#' model = parafac(Fujita2023$data, nfac=1, nstart=1, verbose=FALSE)
#' reinflatedData = reinflateBlock(model)
reinflateBlock = function(model){

  if(methods::is(model, "parafac")){
    loadingVectors = list()
    numComponents = ncol(model$A)
    numModes = length(model$const)

    loadingVectors[[1]] = model$A
    loadingVectors[[2]] = model$B
    loadingVectors[[3]] = model$C
    if(numModes == 4){
      loadingVectors[[4]] = model$D
    }

    model = loadingVectors
  }
  stopifnot(class(model) == "list")
  stopifnot(length(unique(unlist(lapply(model, ncol)))) == 1) # number of components should be equal in all modes

  numComponents = unique(unlist(lapply(model, ncol)))
  dimX = unlist(lapply(model, nrow))

  M = array(0L, dimX)

  # Two-way
  if(length(model) == 2){
    for(i in 1:numComponents){
      A = matrix(model[[1]][,i])
      B = matrix(model[[2]][,i])
      M = M + array(tcrossprod(A, B), dimX)
    }
  }
  # Three-way
  else if(length(model) == 3){
    for(i in 1:numComponents){
      A = matrix(model[[1]][,i])
      B = matrix(model[[2]][,i])
      C = matrix(model[[3]][,i])
      M = M + array(tcrossprod(A, multiway::krprod(C, B)), dimX)
    }
  }
  else{
    stop("reinflateBlock cannot deal with 5-way or higher order arrays.")
  }

  return(M)
}

#' Transform PARAFAC loadings to an orthonormal basis.
#' Note: this function only works for 3-way PARAFAC models.
#'
#' @inheritParams plotPARAFACmodel
#' @param modeToCorrect Correct the subject (1), feature (2) or time mode (3).
#' @param moreOutput Give orthonormal basis and transformation matrices as part of output (default FALSE).
#'
#' @return Corrected loadings of the specified mode.
#' @export
#'
#' @examples
#' library(multiway)
#' library(dplyr)
#' library(ggplot2)
#' library(paramGUI)
#' library(pracma)
#' set.seed(0)
#'
#' # Make PARAFAC model
#' processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, centerMode=1, scaleMode=2)
#' model = parafac(processedFujita$data, nfac=2, nstart=1, verbose=FALSE)
#'
#' transformedA = transformPARAFACloadings(model, 1)
#' plot(transformedA[,1], transformedA[,2])
transformPARAFACloadings = function(model, modeToCorrect, moreOutput=FALSE){

  # This function is purposefully generic about the class of model.
  # It may be either a list of A, B, C or a parafac class object from multiway.
  if(methods::is(model, "parafac")){
    A = model$A
    B = model$B
    C = model$C
  }
  else{
    A = as.matrix(model[[1]])
    B = as.matrix(model[[2]])
    C = as.matrix(model[[3]])
  }

  if(modeToCorrect == 1){
    F = paramGUI::kroneckercol(C, B) %>% as.matrix()
    Ftilde = pracma::gramSchmidt(F)$Q
    T = pracma::pinv(F) %*% Ftilde
    Atilde = A %*% pracma::pinv(t(T))
    result = Atilde
  }
  else if(modeToCorrect == 2){
    F = paramGUI::kroneckercol(A, C) %>% as.matrix()
    Ftilde = pracma::gramSchmidt(F)$Q
    T = pracma::pinv(F) %*% Ftilde
    Btilde = B %*% pracma::pinv(t(T))
    result = Btilde
  }
  else if(modeToCorrect == 3){
    F = paramGUI::kroneckercol(B, A) %>% as.matrix()
    Ftilde = pracma::gramSchmidt(F)$Q
    T = pracma::pinv(F) %*% Ftilde
    Ctilde = C %*% pracma::pinv(t(T))
    result = Ctilde
  }

  if(!moreOutput){
    return(result)
  } else{
    return(list("correctedLoading"=result, "T"=T, "Ftilde"=Ftilde, "F"=F))
  }

}

#' Export the PARAFAC model
#'
#' Currently only supports 3-mode models.
#'
#' @inheritParams plotPARAFACmodel
#' @param prefix Prefix file name
#' @param path Path to a folder
#' @param saveRDS Save an RDS of the PARAFAC model object? (true/false, default=TRUE)
#'
#' @return Saves mode1, mode2, mode3, input data, modelled data, and data as modelled per component in .csv files.
#' @export
#'
#' @examples
#' library(multiway)
#' library(dplyr)
#' library(ggplot2)
#' library(paramGUI)
#' library(pracma)
#' set.seed(0)
#'
#' # Make PARAFAC model
#' processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, centerMode=1, scaleMode=2)
#' model = parafac(processedFujita$data, nfac=2, nstart=1, verbose=FALSE)
#' \dontrun{
#' exportPARAFAC(model, processedFujita, prefix="Fujita")
#' }
#'
exportPARAFAC = function(model, dataset, prefix="PARAFACmodel", path="./", saveRDS=FALSE){
  stopifnot(methods::is(model, "parafac"))

  A = model$A
  B = model$B
  C = model$C
  numComponents = ncol(A)

  mode1 = cbind(A, dataset$mode1) %>% dplyr::as_tibble()
  mode2 = cbind(B, dataset$mode2) %>% dplyr::as_tibble()
  mode3 = cbind(C, dataset$mode3) %>% dplyr::as_tibble()
  input = dataset$data
  modelledData = reinflateBlock(model)

  components = list()
  for(f in 1:numComponents){
    fakeA = matrix(A[,f])
    fakeB = matrix(B[,f])
    fakeC = matrix(C[,f])
    fakeModel = list(fakeA, fakeB, fakeC)
    components[[f]] = reinflateBlock(fakeModel)
  }

  utils::write.table(mode1, paste0(path,prefix,"_mode1.csv"), sep=",", row.names=FALSE, col.names=TRUE)
  utils::write.table(mode2, paste0(path,prefix,"_mode2.csv"), sep=",", row.names=FALSE, col.names=TRUE)
  utils::write.table(mode3, paste0(path,prefix,"_mode3.csv"), sep=",", row.names=FALSE, col.names=TRUE)
  utils::write.table(input, paste0(path,prefix,"_input.csv"), sep=",", row.names=FALSE, col.names=TRUE)
  utils::write.table(modelledData, paste0(path,prefix,"_modelledData.csv"), sep=",", row.names=FALSE, col.names=TRUE)

  for(i in 1:length(components)){
    utils::write.table(components[[i]], paste0(path,prefix,"_component_",i,".csv"), sep=",", row.names=FALSE, col.names=TRUE)
  }

  if(saveRDS == TRUE){
    saveRDS(model, paste0(path,prefix,"_model.RDS"))
  }
}

#' Import PARAFAC model
#'
#' @inheritParams exportPARAFAC
#' @param numComponents Number of components in the model
#' @param sep Separator character for the input files (default=",")
#' @param header Expect headers in the csv files (default=TRUE)
#' @param loadRDS Load a previously saved RDS object of the model (default=TRUE)
#'
#' @return List of: the PARAFAC model, the dataset used, the data as modelled
#' @export
#'
#' @examples
#' library(multiway)
#' library(dplyr)
#' library(ggplot2)
#' library(paramGUI)
#' library(pracma)
#' set.seed(0)
#'
#' # Make PARAFAC model
#' processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, centerMode=1, scaleMode=2)
#' model = parafac(processedFujita$data, nfac=2, nstart=1, verbose=FALSE)
#' \dontrun{
#' exportPARAFAC(model, processedFujita, prefix="Fujita_")
#' importedModel = importPARAFAC(path="./", prefix="Fujita_", numComponents=2)
#' }
#'
importPARAFAC = function(path, prefix, numComponents, sep=",", loadRDS=TRUE, header=TRUE){
  mode1 = utils::read.csv(paste0(path,prefix,"_mode1.csv"), sep=sep, header=header) %>% dplyr::as_tibble()
  mode2 = utils::read.csv(paste0(path,prefix,"_mode2.csv"), sep=sep, header=header) %>% dplyr::as_tibble()
  mode3 = utils::read.csv(paste0(path,prefix,"_mode3.csv"), sep=sep, header=header) %>% dplyr::as_tibble()
  input = utils::read.csv(paste0(path,prefix,"_input.csv"), sep=sep, header=header) %>% dplyr::as_tibble()
  modelledData = utils::read.csv(paste0(path,prefix,"_model.csv"), sep=sep, header=header) %>% dplyr::as_tibble()

  components = list()
  for(i in 1:length(numComponents)){
    components[[i]] = utils::read.csv(paste0(path,prefix,"_component_",i,".csv"), sep=sep, header=header) %>% dplyr::as_tibble()
  }

  dataset = list("data"=input, "mode1"=mode1[,(numComponents+1):ncol(mode1)], "mode2"=mode2[,(numComponents+1):ncol(mode2)], "mode3"=mode3[,(numComponents+1):ncol(mode3)])

  if(loadRDS){
    model = readRDS(paste0(path,prefix,"_model.RDS"))
  }
  else{
    model = list("A"=mode1[,1:numComponents], "B"=mode2[,1:numComponents], "C"=mode3[,1:numComponents])
  }

  output = list("model"=model, "dataset"=dataset, "reconstructedData"=modelledData, "reconstructedPerComponent"=components)
}
