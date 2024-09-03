#' Core Consistency Diagnostic (CORCONDIA) calculation
#'
#' @param X Input data matrix
#' @param Fac PARAFAC model Fac object
#'
#' @return Scalar of the CORCONDIA value
#' @export
#'
#' @examples
#' X = Fujita2023$data
#' model = parafac(X, 2)
#' corcondia(X, model$Fac)
corcondia = function (X, Fac)
{
  numComponents = ncol(Fac[[1]])

  # Put imputed values in place of missing values to be robust against NAs.
  if(any(is.na(X))){
    Xhat = reinflateTensor(Fac[[1]], Fac[[2]], Fac[[3]])
    Xnew = X
    Xnew[is.na(X)] = Xhat[is.na(X)]
    X = Xnew
  }

  # Create core array G
  g = tcrossprod(pracma::pinv(Fac[[1]]) %*% matrix(X, nrow=nrow(Fac[[1]])), kronecker(pracma::pinv(Fac[[3]]), pracma::pinv(Fac[[2]])))
  G = array(g, dim = rep(numComponents,3))

  # Create ideal G with super-diagonal of 1
  super = array(0, dim = rep(numComponents,3))
  for(k in 1:numComponents){
    super[k,k,k] = 1
  }

  # Calculate CORCONDIA as described in Bro, Kiers, 2003
  result = 100 * (1 - sum((G - super)^2)/numComponents)
  return(result)
}
