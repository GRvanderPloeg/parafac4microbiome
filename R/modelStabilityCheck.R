#' Jack-knifing procedure to determine PARAFAC model stability.
#'
#' @inheritParams calculateSparsity
#' @inheritParams plotPARAFACmodel
#' @param numComponents Number of components of the desired PARAFAC model
#' @param numFolds Number of jack-knifed operations to perform
#' @param ctol Change in SSQ needed for model to be converged (default 1e-6).
#' @param maxit Maximum number of iterations (default 2500).
#' @param numCores Number of cores to use. If set larger than 1, it will run the job in parallel (default 1)
#'
#' @return Stabilized sign-flipped jack-knifed PARAFAC models as well as a plot of the median model with error bars.
#' @export
#' @importFrom foreach %dopar%
#'
#' @examples
#' processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, centerMode=1, scaleMode=2)
#' modelStability = modelStabilityCheck(processedFujita, numComponents=3)
#'
modelStabilityCheck = function(dataset, numComponents=1, numFolds=nrow(X), considerGroups=FALSE, groupVariable="",
                               colourCols=NULL, legendTitles=NULL, xLabels=NULL, legendColNums=NULL, arrangeModes=NULL,
                               continuousModes=NULL, ctol=1e-6, maxit=2500, numCores=1){

  if(considerGroups == TRUE & groupVariable == ""){
    warning("When setting considerGroups to TRUE, please also specify a groupVariable.")
    return(0)
  } else if(considerGroups == FALSE & groupVariable != ""){
    considerGroups = TRUE
  }

  X = dataset$data
  sampleMetadata = dataset$mode1
  numSamples = nrow(X)
  numModes = length(dim(X))
  samplesToRemove = list()

  # Convert default settings to usable content.
  if(is.null(colourCols)){
    colourCols = rep("", numModes)
  }
  if(is.null(legendTitles)){
    legendTitles = rep("", numModes)
  }
  if(is.null(xLabels)){
    xLabels = rep("", numModes)
  }
  if(is.null(legendColNums)){
    legendColNums = rep(0, numModes)
  }
  if(is.null(arrangeModes)){
    arrangeModes = rep(FALSE, numModes)
  }
  if(is.null(continuousModes)){
    continuousModes = rep(FALSE, numModes)
  }

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
  if(numCores > 1){
    cl = parallel::makeCluster(numCores)
    doParallel::registerDoParallel(cl)
    models = foreach::foreach(i=1:numFolds) %dopar% {
      model = parafac4microbiome::parafac(X[-samplesToRemove[[i]],,], nfac=numComponents, nstart=1, ctol=ctol, maxit=maxit, verbose=FALSE)
    }
    parallel::stopCluster(cl)
  } else{
    models = list()
    for(i in 1:numFolds){
      models[[i]] = parafac4microbiome::parafac(X[-samplesToRemove[[i]],,], nfac=numComponents, nstart=1, ctol=ctol, maxit=maxit, verbose=FALSE)
    }
  }

  # Modify subject loadings to reflect a missing sample
  for(i in 1:numFolds){
    model = models[[i]]
    mask = 1:nrow(X) %in% samplesToRemove[[i]]
    temp = matrix(0L, nrow(X), numComponents)
    temp[mask,] = NA
    temp[!mask,] = model$Fac[[1]]
    model$Fac[[1]] = temp

    models[[i]] = model
  }

  # Fix sign flipping of components
  models = flipLoadings(models, X)

  # Add a plot of the loadings to the output
  overallTitle = paste0("Jack-knifed models, numFolds=", numFolds)
  plot = plotModelStability(models, dataset, colourCols, legendTitles, xLabels, legendColNums, arrangeModes, continuousModes, overallTitle)

  return(list("models"=models, "plot"=plot))
}
