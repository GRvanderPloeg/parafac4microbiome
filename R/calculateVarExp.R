#' Calculate variance explained of a PARAFAC model
#'
#'
#' @param model PARAFAC model output from the [parafac4microbiome::parafac()] function.
#' @param X Input data of the PARAFAC model.
#'
#' @return The variance explained of the model, expressed as a fraction (between 0-1).
#' @export
#'
#' @examples
#' X = Fujita2023$data
#' model = parafac(X, nfac=1, nstart=1, verbose=FALSE)
#' calculateVarExp(model, X)
calculateVarExp = function(model, X){
  Xhat = reinflateBlock(model)
  # Replace imputed values with NA to avoid incorrect calculation
  Xhat[is.na(X)] = NA
  varExp = multiway::sumsq(Xhat, na.rm=TRUE) / multiway::sumsq(X, na.rm=TRUE)
  return(varExp)
}
