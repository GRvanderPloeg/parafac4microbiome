#' Create randomly initialized models for determining the correct number of components.
#'
#' @param X Input data
#' @param minNumComponents Minimum number of components (default 1).
#' @param maxNumComponents Maximum number of components (default 5).
#' @param numRepetitions Number of randomly initialized models to create (default 100).
#' @param ctol Change in SSQ needed for model to be converged (default 1e-6).
#' @param maxit Maximum number of iterations (default 2500).
#' @param numCores Number of cores to use. If set larger than 1, it will run the job in parallel (default 1)
#'
#' @return A list object of the following:
#' * plot: Plots of all assessed metrics and an overview plot showing a summary of all of them.
#' * metrics: metrics of every created model (number of iterations, sum of squared errors, CORCONDIA score and variance explained).
#' * models: all created models.
#'
#' @export
#'
#' @examples
#' X = Fujita2023$data
#' assessment = assessNumComponents(X, minNumComponents=1, maxNumComponents=3, numRepetitions=5)
#' assessment$plots$overview
assessNumComponents = function(X, minNumComponents=1, maxNumComponents=5, numRepetitions=100, ctol=1e-6, maxit=2500, numCores=1){

  metrics = list()
  allModels = list()
  numModes = length(dim(X))

  names = list(1:numRepetitions, minNumComponents:maxNumComponents) #list(1:numRepetitions, paste0(minNumComponents:maxNumComponents, " components"))
  numIterations = matrix(0L, nrow=numRepetitions, ncol=length(minNumComponents:maxNumComponents), dimnames=names)
  SSE = matrix(0L, nrow=numRepetitions, ncol=length(minNumComponents:maxNumComponents), dimnames=names)
  CORCONDIA = matrix(0L, nrow=numRepetitions, ncol=length(minNumComponents:maxNumComponents), dimnames=names)
  TCC = list()
  varExp = matrix(0L, nrow=numRepetitions, ncol=length(minNumComponents:maxNumComponents), dimnames=names)

  for(f in minNumComponents:maxNumComponents){

    if(numCores > 1){
      cl = parallel::makeCluster(numCores)
      doParallel::registerDoParallel(cl)
      parallel::clusterSetRNGStream(cl, 1)
      models = foreach::foreach(i=1:numRepetitions) %dopar% {
        model=parafac4microbiome::parafac(X, nfac=f, nstart=1, ctol=ctol, maxit=maxit, verbose=FALSE)
      }
      parallel::stopCluster(cl)
    } else{
      models = parafac(X, nfac=f, nstart=numRepetitions, maxit=maxit, ctol=ctol, output ="all", verbose=FALSE)
    }

    numIterations[,f] = sapply(models, function(model){model$iter})
    SSE[,f] = sapply(models, function(model){model$SSE})
    CORCONDIA[,f] = sapply(models, function(model){corcondia(X, model)})
    varExp[,f] = sapply(models, function(model){model$Rsq*100})
    allModels[[f]] = models

    TCC[[f]] = array(rep(0, f*f*numRepetitions*numModes), dim=c(f,f,numRepetitions,numModes))
    for(i in 1:numModes){
      for(j in 1:f){
        for(k in 1:f){
          for(l in 1:numRepetitions){
            model = models[[l]]
            convertedModel = convertModelFormat(model)
            TCC[[f]][j,k,l,i] = multiway::congru(convertedModel[[i]][,j], convertedModel[[i]][,k])
          }
        }
      }
    }
  }

  metrics = list("numIterations"=numIterations,
                 "SSE"=SSE,
                 "CORCONDIA"=CORCONDIA,
                 "varExp"=varExp,
                 "TCC"=TCC)

  plots = plotModelMetric(metrics)
  return(list("plots"=plots, "metrics"=metrics, "models"=allModels))
}
