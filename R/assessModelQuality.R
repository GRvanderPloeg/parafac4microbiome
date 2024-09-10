#' Create randomly initialized models to determine the correct number of components by assessing model quality metrics.
#'
#' @inheritParams parafac
#' @param X Input data
#' @param minNumComponents Minimum number of components (default 1).
#' @param maxNumComponents Maximum number of components (default 5).
#' @param numRepetitions Number of randomly initialized models to create (default 100).
#' @param ctol Change in SSQ needed for model to be converged (default 1e-6).
#' @param maxit Maximum number of iterations (default 2500).
#' @param numCores Number of cores to use. If set larger than 1, it will run the job in parallel (default 1)
#'
#' @return A list object of the following:
#' * plots: Plots of all assessed metrics and an overview plot showing a summary of all of them.
#' * metrics: metrics of every created model (number of iterations, sum of squared errors, CORCONDIA score and variance explained).
#' * models: all created models.
#'
#' @export
#'
#' @examples
#' X = Fujita2023$data
#' assessment = assessModelQuality(X, minNumComponents=1, maxNumComponents=3, numRepetitions=5)
#' assessment$plots$overview
assessModelQuality = function(X, minNumComponents=1, maxNumComponents=5, numRepetitions=100, method="als", ctol=1e-6, maxit=2500, max_fn=10000, rel_tol=1e-8, abs_tol=1e-8, grad_tol=1e-8, numCores=1){

  metrics = list()
  allModels = list()
  numModes = length(dim(X))

  names = list(1:numRepetitions, minNumComponents:maxNumComponents) #list(1:numRepetitions, paste0(minNumComponents:maxNumComponents, " components"))
  numIterations = matrix(0L, nrow=numRepetitions, ncol=length(minNumComponents:maxNumComponents), dimnames=names)
  SSE = matrix(0L, nrow=numRepetitions, ncol=length(minNumComponents:maxNumComponents), dimnames=names)
  CORCONDIA = matrix(0L, nrow=numRepetitions, ncol=length(minNumComponents:maxNumComponents), dimnames=names)
  varExp = matrix(0L, nrow=numRepetitions, ncol=length(minNumComponents:maxNumComponents), dimnames=names)

  plots = list()
  plots$TCC = list()

  for(f in minNumComponents:maxNumComponents){

    # Run PARAFAC models
    if(numCores > 1){
      cl = parallel::makeCluster(numCores)
      doParallel::registerDoParallel(cl)
      models = foreach::foreach(i=1:numRepetitions) %dopar% {
        model=parafac4microbiome::parafac(X, nfac=f, nstart=1, method=method, ctol=ctol, rel_tol=rel_tol, abs_tol=abs_tol, grad_tol=grad_tol, maxit=maxit, max_fn=max_fn, verbose=FALSE)
      }
      parallel::stopCluster(cl)
    } else{
      models = parafac(X, nfac=f, nstart=numRepetitions, method=method, ctol=ctol, rel_tol=rel_tol, abs_tol=abs_tol, grad_tol=grad_tol, maxit=maxit, max_fn=max_fn, output ="all", verbose=FALSE)
    }

    # Store output
    numIterations[,f] = sapply(models, function(model){model$iter})
    SSE[,f] = sapply(models, function(model){model$SSE})
    CORCONDIA[,f] = sapply(models, function(model){corcondia(X, model$Fac)})
    varExp[,f] = sapply(models, function(model){model$varExp})
    allModels[[f]] = models

    # Plot TCC for this number of components
    plots$TCC[[f]] = plotModelTCCs(models)
  }

  # Plot the other metrics
  plots$numIterations = plotModelMetric(numIterations, plottingMode="bar", ylabel="Number of iterations")
  plots$SSE = plotModelMetric(SSE, plottingMode="bar", ylabel="SSE")
  plots$CORCONDIA = plotModelMetric(CORCONDIA, plottingMode="bar", ylabel="CORCONDIA")
  plots$varExp = plotModelMetric(varExp, plottingMode="bar", ylabel="Variation explained (%)")
  plots$overview = ggpubr::ggarrange(plotModelMetric(numIterations, ylabel="Number of iterations"),
                                     plotModelMetric(SSE, ylabel="SSE"),
                                     plotModelMetric(CORCONDIA, ylabel="CORCONDIA"),
                                     plotModelMetric(varExp, ylabel="Variation explained (%)"))

  metrics = list("numIterations"=numIterations,
                 "SSE"=SSE,
                 "CORCONDIA"=CORCONDIA,
                 "varExp"=varExp)

  return(list("plots"=plots, "metrics"=metrics, "models"=allModels))
}
