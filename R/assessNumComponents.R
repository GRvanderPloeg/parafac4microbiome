#' Create randomly initialized models for component selection
#'
#' @param X Input data
#' @param minNumComponents Minimum number of components (default 1).
#' @param maxNumComponents Maximum number of components (default 5).
#' @param numRepetitions Number of randomly initialized models to create (default 100).
#'
#' @return List object of the following:
#' * metrics: metrics of every create model (number of iterations, sum of squared errors, CORCONDIA score and variance explained).
#' * models: all created models.
#'
#' @export
#'
#' @examples
#' X = Fujita2023$data
#' output = assessNumComponents(X)
assessNumComponents = function(X, minNumComponents=1, maxNumComponents=5, numRepetitions=100){

  metrics = list()
  allModels = list()

  names = list(1:numRepetitions, paste0(minNumComponents:maxNumComponents, " components"))
  numIterations = matrix(0L, nrow=numRepetitions, ncol=length(minNumComponents:maxNumComponents), dimnames=names)
  SSE = matrix(0L, nrow=numRepetitions, ncol=length(minNumComponents:maxNumComponents), dimnames=names)
  CORCONDIA = matrix(0L, nrow=numRepetitions, ncol=length(minNumComponents:maxNumComponents), dimnames=names)
  varExp = matrix(0L, nrow=numRepetitions, ncol=length(minNumComponents:maxNumComponents), dimnames=names)

  for(f in minNumComponents:maxNumComponents){
    models = multiway::parafac(X, nfac=f, nstart=numRepetitions, output ="all", verbose=FALSE)

    numIterations[,f] = sapply(models, function(model){model$iter})
    SSE[,f] = sapply(models, function(model){model$SSE})
    CORCONDIA[,f] = sapply(models, function(model){calculateCORCONDIA(X,model)})
    varExp[,f] = sapply(models, function(model){model$Rsq*100})
    allModels[[f]] = models
  }

  metrics = list("numIterations"=numIterations,
                 "SSE"=SSE,
                 "CORCONDIA"=CORCONDIA,
                 "varExp"=varExp)

  return(list("metrics"=metrics, "models"=allModels))
}
