#' Jack-knifed PARAFAC models to determine model stability
#'
#' @inheritParams calculateSparsity
#' @param X Input data cube
#' @param sampleMetadata Input sample metadata
#' @param numComponents Number of components of the desired PARAFAC model
#' @param numFolds Number of jack-knifed samples to create
#' @param ctol Change in SSQ needed for model to be converged (default 1e-6).
#' @param maxit Maximum number of iterations (default 2500).
#' @param numCores Number of cores to use. If set larger than 1, it will run the job in parallel (default 1)
#'
#' @return List of all As, Bs, Cs of the PARAFAC models.
#' @export
#' @importFrom foreach %dopar%
#'
#' @examples
#' processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, centerMode=1, scaleMode=2)
#' modelStability = modelStabilityCheck(processedFujita$data, processedFujita$mode1, numComponents=3)
modelStabilityCheck = function(X, sampleMetadata, numComponents=1, numFolds=nrow(X),
                               considerGroups=FALSE, groupVariable="", ctol=1e-6, maxit=2500, numCores=1){
  # TODO: catch considerGroups=TRUE but groupVariable is "" or the other way around

  numSamples = nrow(X)
  numModes = length(dim(X))
  samplesToRemove = list()

  # Determine which samples to remove per fold
  if(considerGroups == TRUE & groupVariable %in% colnames(sampleMetadata)){
    groupNames = unique(sampleMetadata[groupVariable]) %>% dplyr::pull()
    numGroups = length(groupNames)
    drawIndices = list()

    # Determine the row indices for all samples per group
    for(i in 1:numGroups){
      df = sampleMetadata %>% dplyr::mutate(index=dplyr::row_number())
      mask = df[groupVariable] == groupNames[i]
      drawIndices[[i]] = df[mask,"index"] %>% dplyr::pull()
    }

    for(i in 1:numFolds){
      indices = c()
      for(j in 1:numGroups){
        indices = c(indices, sample(drawIndices[[j]], 1)) # draw one sample from each group
      }
      samplesToRemove[[i]] = indices
    }
  }
  else{
    if(numFolds == nrow(X)){
      for(i in 1:numFolds){samplesToRemove[[i]] = c(i)} # cut out every sample once
    }
    else if(numFolds != nrow(X)){
      for(i in 1:numFolds){samplesToRemove[[i]] = sample(1:nrow(X), 1)} # cut one sample randomly at a time
    }
  }

  # Create jack-knifed PARAFAC models - parallelized
  cl = parallel::makeCluster(numCores)
  doParallel::registerDoParallel(cl)
  models = foreach::foreach(i=1:numFolds) %dopar% {
    library(multiway)
    library(parafac4microbiome)
    model=parafac(X[-samplesToRemove[[i]],,], nfac=numComponents, nstart=1, ctol=ctol, maxit=maxit, verbose=FALSE)
  }
  parallel::stopCluster(cl)

  # Modify subject loadings to reflect a missing sample
  for(i in 1:numFolds){
    model = models[[i]]
    mask = 1:nrow(X) %in% samplesToRemove[[i]]
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
  stabilizedA = list()
  stabilizedB = list()
  stabilizedC = list()
  for(i in 1:numComponents){
    evidenceA = checkForFlippedLoadings(A[[i]])
    evidenceB = checkForFlippedLoadings(B[[i]])
    evidenceC = checkForFlippedLoadings(C[[i]])
    evidenceMatrix = rbind(evidenceA, evidenceB, evidenceC)

    repairedLoadings = repairLoadings(A[[i]], B[[i]], C[[i]], evidenceMatrix)
    stabilizedA[[i]] = repairedLoadings[[1]]
    stabilizedB[[i]] = repairedLoadings[[2]]
    stabilizedC[[i]] = repairedLoadings[[3]]
  }

  return(list(stabilizedA, stabilizedB, stabilizedC))
}
