#' Bootstrapping procedure to determine PARAFAC model stability for a given number of components.
#'
#' @inheritParams calculateSparsity
#' @inheritParams plotPARAFACmodel
#' @inheritParams parafac
#' @inheritParams assessModelQuality
#' @param numFolds Number of bootstrapped models to create.
#'
#' @return A list containing the following:
#' * models: All stabilized sign-flipped bootstrapped PARAFAC models.
#' * modelPlots: A list of plots of the median model with error bars for each number of components.
#' * FMSplot: A bar plot showing the Factor Match Scores per number of components (see Li et al., 2024).
#' * FMS: FMS values that the FMS plot is based on.
#'
#' @export
#' @importFrom foreach %dopar%
#'
#' @examples
#' processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, centerMode=1, scaleMode=2)
#' modelStability = assessModelStability(processedFujita, minNumComponents=1, maxNumComponents=3)
assessModelStability = function(dataset, minNumComponents=1, maxNumComponents=5, numFolds=dim(dataset$data)[1], considerGroups=FALSE, groupVariable="",
                               colourCols=NULL, legendTitles=NULL, xLabels=NULL, legendColNums=NULL, arrangeModes=NULL, method="als", ctol=1e-6,
                               maxit=2500, max_fn=10000, rel_tol=1e-8, abs_tol=1e-8, grad_tol=1e-8, numCores=1){

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

  allModels = list()
  allModelPlots = list()
  allFMS = list()

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

  # Determine which samples to remove per fold
  samplesToRemove = list()
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

  # Create bootstrapped PARAFAC models are store the results
  for(f in minNumComponents:maxNumComponents){

    # Create jack-knifed PARAFAC models - parallelized
    if(numCores > 1){
      cl = parallel::makeCluster(numCores)
      doParallel::registerDoParallel(cl)
      models = foreach::foreach(i=1:numFolds) %dopar% {
        model = parafac4microbiome::parafac(X[-samplesToRemove[[i]],,], nfac=f, initialization="nvec", nstart=1, ctol=ctol, maxit=maxit, verbose=FALSE)
      }
      parallel::stopCluster(cl)
    } else{
      models = list()
      for(i in 1:numFolds){
        models[[i]] = parafac4microbiome::parafac(X[-samplesToRemove[[i]],,], nfac=f, initialization="nvec", nstart=1, ctol=ctol, maxit=maxit, verbose=FALSE)
      }
    }

    # Modify subject loadings to reflect a missing sample
    for(i in 1:numFolds){
      model = models[[i]]
      mask = 1:nrow(X) %in% samplesToRemove[[i]]
      temp = matrix(0L, nrow(X), f)
      temp[mask,] = NA
      temp[!mask,] = model$Fac[[1]]
      model$Fac[[1]] = temp

      models[[i]] = model
    }

    # Fix sign flipping of components
    models = flipLoadings(models, X)

    # Add a plot of the loadings to the output
    overallTitle = paste0("Jack-knifed models, numFolds=", numFolds)
    continuousModes = rep(FALSE, numModes)
    allModelPlots[[f]] = plotModelStability(models, dataset, colourCols, legendTitles, xLabels, legendColNums, arrangeModes, continuousModes, overallTitle)

    allModels[[f]] = models
    allFMS[[f]] = calculateFMS(models)
  }

  allFMS = simplify2array(allFMS)
  FMSplot = plotModelMetric(allFMS, plottingMode="box", ylabel="FMS")
  return(list("models"=allModels, "modelPlots"=allModelPlots, "FMSplot"=FMSplot, "FMS"=allFMS))
}
