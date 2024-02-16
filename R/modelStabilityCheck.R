#' Jack-knifed PARAFAC models to determine model stability
#'
#' @param X Input data cube
#' @param sampleMetadata Input sample metadata
#' @param numComponents Number of components of the desired PARAFAC model
#' @param numRepetitions Number of jack-knifed samples to create
#'
#' @return List of all As, Bs, Cs of the PARAFAC models.
#' @export
#'
#' @examples
#' modelStability = modelStabilityCheck(Fujita2023$data, Fujita2023$sampleMetadata, numComponents=3)
modelStabilityCheck = function(X, sampleMetadata, numComponents=1, numRepetitions=nrow(X)){

  numSamples = nrow(X)
  numModes = length(dim(X))

  models = list()
  for(i in 1:numRepetitions){
    df = X[-i,,]
    model = multiway::parafac(df, nfac=numComponents, nstart=1, verbose=FALSE)

    # Modify subject loadings to reflect a missing sample
    mask = 1:nrow(X) == i
    temp = matrix(0L, nrow(X), numComponents)
    temp[mask,] = NA
    temp[!mask,] = model$A
    model$A = temp

    models[[i]] = model
  }

  A = list()
  B = list()
  C = list()
  for(i in 1:numComponents){
    A[[i]] = simplify2array(lapply(models, function(x){x$A[,i]}))
    B[[i]] = simplify2array(lapply(models, function(x){x$B[,i]}))
    C[[i]] = simplify2array(lapply(models, function(x){x$C[,i]}))
  }

  return(list(A,B,C))
}
