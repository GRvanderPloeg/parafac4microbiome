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
#' DEPRECATED: use [reinflateTensor()] instead.
#'
#' @param Fac Fac object containing the components from a PARAFAC model, see [parafac()].
#'
#' @return Multi-way array of the reconstructed data.
#' @export
#'
#' @examples
#' model = parafac(Fujita2023$data, nfac=1, nstart=1, verbose=FALSE)
#' reinflatedData = reinflateBlock(model$Fac)
reinflateBlock = function(Fac){
  return(reinflateTensor(Fac[[1]], Fac[[2]], Fac[[3]]))
}

#' Transform PARAFAC loadings to an orthonormal basis.
#' Note: this function only works for 3-way PARAFAC models.
#'
#' @param Fac Fac object from a PARAFAC object, see [parafac()].
#' @param modeToCorrect Correct the subject (1), feature (2) or time mode (3).
#' @param moreOutput Give orthonormal basis and transformation matrices as part of output (default FALSE).
#'
#' @return Corrected loadings of the specified mode.
#' @export
#'
#' @examples
#' processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, centerMode=1, scaleMode=2)
#' model = parafac(processedFujita$data, nfac=2, nstart=1, verbose=FALSE)
#' transformedA = transformPARAFACloadings(model$Fac, 1)
transformPARAFACloadings = function(Fac, modeToCorrect, moreOutput=FALSE){

  A = as.matrix(Fac[[1]])
  B = as.matrix(Fac[[2]])
  C = as.matrix(Fac[[3]])

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

#' Create a tensor out of a set of matrices similar to a component model.
#'
#' @param A I x N matrix corresponding to loadings in the first mode for N components.
#' @param B J x N matrix corresponding to loadings in the second mode for N components.
#' @param C K x N matrix corresponding to loadings in the third mode for N components.
#' @param returnAsTensor Boolean return as [rTensor] S4 tensor object (default FALSE).
#'
#' @return M, an I x J x K tensor.
#' @export
#'
#' @examples
#' A = rnorm(108)
#' B = rnorm(100)
#' C = rnorm(10)
#' M = reinflateTensor(A,B,C)
reinflateTensor = function(A, B, C, returnAsTensor=FALSE){

  # Try to cast to matrix if the input is different
  if(!methods::is(A, "matrix")){
    A = as.matrix(A)
  }
  if(!methods::is(B, "matrix")){
    B = as.matrix(B)
  }
  if(!methods::is(C, "matrix")){
    C = as.matrix(C)
  }

  I = nrow(A)
  J = nrow(B)
  K = nrow(C)
  reinflatedTensor = array(tcrossprod(A, multiway::krprod(C, B)), c(I,J,K))

  if(returnAsTensor == TRUE){
    reinflatedTensor = rTensor::as.tensor(reinflatedTensor)
  }
  else{
    reinflatedTensor = reinflatedTensor
  }

  return(reinflatedTensor)
}

#' Sum-of-squares calculation
#'
#' @param X Either a list containing matrices, or a matrix of values.
#' @param na.rm Remove NAs from calculation (default FALSE).
#'
#' @return sum-of-squares of the object
#' @export
#'
#' @examples
#' X = array(rnorm(108*100*10), c(108,100,10))
#' ssq = sumsqr(X)
sumsqr = function(X, na.rm=FALSE){

  if(methods::is(X, "list")){
    result = sum(sapply(X, sumsqr, na.rm=na.rm)) # recursive to iterate over list
  } else{
    result = sum(X^2, na.rm=na.rm)
  }

  return(result)
}
