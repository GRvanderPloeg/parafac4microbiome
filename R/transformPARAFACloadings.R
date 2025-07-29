#' Transform PARAFAC loadings to an orthonormal basis.
#' Note: this function only works for 3-way PARAFAC models.
#'
#' @param Fac Fac object from a PARAFAC object, see [parafac()].
#' @param modeToCorrect Correct the subject (1), feature (2) or time mode (3).
#' @param moreOutput Give orthonormal basis and transformation matrices as part of output (default FALSE).
#'
#' @return Corrected loadings of the specified mode.
#' @export
#'
#' @examples
#' processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, centerMode=1, scaleMode=2)
#' model = parafac(processedFujita$data, nfac=2, nstart=1)
#' transformedA = transformPARAFACloadings(model$Fac, 1)
transformPARAFACloadings = function(Fac, modeToCorrect, moreOutput=FALSE){
  Fac = lapply(Fac, as.matrix)
  A = Fac[[1]]
  B = Fac[[2]]
  C = Fac[[3]]

  if(modeToCorrect == 1){
    F = multiway::krprod(C, B) %>% as.matrix()
    Ftilde = pracma::gramSchmidt(F)$Q
    T = pracma::pinv(F) %*% Ftilde
    Atilde = A %*% pracma::pinv(t(T))
    result = Atilde
  }
  else if(modeToCorrect == 2){
    F = multiway::krprod(A, C) %>% as.matrix()
    Ftilde = pracma::gramSchmidt(F)$Q
    T = pracma::pinv(F) %*% Ftilde
    Btilde = B %*% pracma::pinv(t(T))
    result = Btilde
  }
  else if(modeToCorrect == 3){
    F = multiway::krprod(B, A) %>% as.matrix()
    Ftilde = pracma::gramSchmidt(F)$Q
    T = pracma::pinv(F) %*% Ftilde
    Ctilde = C %*% pracma::pinv(t(T))
    result = Ctilde
  }

  if(!moreOutput){
    return(result)
  } else{
    return(list("correctedLoading"=result, "T"=T, "Ftilde"=Ftilde, "F"=F))
  }

}
