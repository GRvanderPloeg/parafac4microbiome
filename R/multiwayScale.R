#' Scale a multi-way array
#'
#' @param X Multi-way array
#' @param mode Mode to scale within: 1=subjects,2=features,3=time (default 2).
#'
#' @return Scaled multi-way array
#' @export
#'
#' @examples
#' cube_scl = multiwayCenter(Fujita2023$data)
multiwayScale = function(X, mode=2){

  if(!methods::is(X, "Tensor")){
    X = rTensor::as.tensor(X)
  }

  unfoldedX = rTensor::k_unfold(X, mode)@data
  stds = apply(unfoldedX, 1, function(x){stats::sd(x, na.rm=TRUE)})
  unfoldedX_scl = sweep(unfoldedX, 1, stds, FUN="/")
  X_scl = rTensor::k_fold(unfoldedX_scl, m=mode, modes=X@modes)
  return(X_scl@data)
}
