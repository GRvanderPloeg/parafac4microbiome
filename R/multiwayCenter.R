#' Center a multi-way array
#'
#' @param X Multi-way array
#' @param mode Mode to center across (default 1).
#'
#' @return Centered multi-way array
#' @export
#'
#' @examples
#' cube_cnt = multiwayCenter(Fujita2023$data)
multiwayCenter = function(X, mode=1){

  if(!methods::is(X, "Tensor")){
    X = rTensor::as.tensor(X)
  }

  unfoldedX = rTensor::k_unfold(X, mode)@data
  unfoldedX_cnt = sweep(unfoldedX, 2, colMeans(unfoldedX, na.rm=TRUE), FUN="-")
  X_cnt = rTensor::k_fold(unfoldedX_cnt, m=mode, modes=X@modes)
  return(X_cnt@data)
}
