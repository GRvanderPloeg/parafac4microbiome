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
  Xhat = reinflateBlock(Fac)
  # Replace imputed values with NA to avoid incorrect calculation
  Xhat[is.na(X)] = NA
  varExp = sumsqr(Xhat, na.rm=TRUE) / sumsqr(X, na.rm=TRUE)
  return(varExp)
}
