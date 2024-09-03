#' Perform a centered log-ratio transform over a multi-way array
#'
#' Note: Propagates NAs corresponding to missing samples.
#'
#' @param X Multi-way array of counts
#' @param pseudocount Pseudocount value to use (default 1).
#'
#' @return CLRed cube
#' @export
#'
#' @examples
#' cubeCLR = multiwayCLR(Fujita2023$data)
multiwayCLR = function(X, pseudocount=1){

  if(!methods::is(X, "Tensor")){
    X = rTensor::as.tensor(X)
  }

  unfoldedX = t(rTensor::k_unfold(X, 2)@data) # unfold to J x IK matrix, then transpose
  unfoldedX_clr = t(apply(unfoldedX+1, 1, function(x){log(x / compositions::geometricmean(x))}))
  X_clr = rTensor::k_fold(t(unfoldedX_clr), m=2, X@modes)
  return(X_clr@data)
}
