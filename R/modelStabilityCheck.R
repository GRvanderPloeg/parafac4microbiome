#' Jack-knifed PARAFAC models to determine model stability
#'
#' @param X Input data cube
#' @param sampleMetadata Input sample metadata
#' @param numComponents Number of components of the desired PARAFAC model
#' @param numRepetitions Number of jack-knifed samples to create
#' @param ctol Change in SSQ needed for model to be converged (default 1e-6).
#' @param maxit Maximum number of iterations (default 2500).
#'
#' @return List of all As, Bs, Cs of the PARAFAC models.
#' @export
#'
#' @examples
#' processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, centerMode=1, scaleMode=2)
#' modelStability = modelStabilityCheck(processedFujita$data, processedFujita$mode1, numComponents=3)
modelStabilityCheck = function(X, sampleMetadata, numComponents=1, numRepetitions=nrow(X), ctol=1e-6, maxit=2500){

  numSamples = nrow(X)
  numModes = length(dim(X))
  samplesToRemove = list()

  if(numRepetitions == nrow(X)){
    for(i in 1:numRepetitions){samplesToRemove[[i]] = c(i)}
  }
  if(numRepetitions != nrow(X)){
    for(i in 1:numRepetitions){samplesToRemove[[i]] = sample(1:nrow(X), 1)}
  }

  # Create jack-knifed PARAFAC models
  models = list()
  for(i in 1:numRepetitions){
    removeSamples = samplesToRemove[[i]]
    df = X[-removeSamples,,]
    model = parafac(df, nfac=numComponents, nstart=1, ctol=ctol, maxit=maxit, verbose=FALSE)

    # Modify subject loadings to reflect a missing sample
    mask = 1:nrow(X) %in% removeSamples
    temp = matrix(0L, nrow(X), numComponents)
    temp[mask,] = NA
    temp[!mask,] = model$A
    model$A = temp

    models[[i]] = model
  }

  # Store the output
  A = list()
  B = list()
  C = list()
  for(i in 1:numComponents){
    A[[i]] = simplify2array(lapply(models, function(x){x$A[,i]}))
    B[[i]] = simplify2array(lapply(models, function(x){x$B[,i]}))
    C[[i]] = simplify2array(lapply(models, function(x){x$C[,i]}))
  }

  # Fix the issue where randomly some components/modes have flipped signs
  # Uses non-exported helper functions in utils.R
  for(i in 1:numComponents){
    evidenceA = checkForFlippedLoadings(A[[i]])
    evidenceB = checkForFlippedLoadings(B[[i]])
    evidenceC = checkForFlippedLoadings(C[[i]])
    evidenceMatrix = rbind(evidenceA, evidenceB, evidenceC)

    repairedLoadings = repairLoadings(A[[i]], B[[i]], C[[i]], evidenceMatrix)
    A[[i]] = repairedLoadings[[1]]
    B[[i]] = repairedLoadings[[2]]
    C[[i]] = repairedLoadings[[3]]
  }

  return(list(A,B,C))
}
