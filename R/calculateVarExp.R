#' Calculate the variation explained by a PARAFAC model.
#'
#' @param Fac Fac object output from the [parafac()] function.
#' @param X Input data of the PARAFAC model.
#'
#' @return The variation explained by the model, expressed as a fraction (between 0-1).
#' @export
#'
#' @examples
#' X = Fujita2023$data
#' model = parafac(X, nfac=1, nstart=1, verbose=FALSE)
#' calculateVarExp(model$Fac, X)
calculateVarExp = function(Fac, X){
  Fac = lapply(Fac, as.matrix) # protection from the 1-component case

  Xhat = reinflateTensor(Fac[[1]], Fac[[2]], Fac[[3]])
  Xhat[is.na(X)] = 0 # Replace imputed values with 0 to avoid adding them to SSQ
  varExp = sumsqr(Xhat, na.rm=TRUE) / sumsqr(X, na.rm=TRUE)
  return(varExp)
}
