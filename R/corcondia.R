#' Core Consistency Diagnostic
#'
#' Calculates Bro and Kiers's core consistency diagnostic (CORCONDIA) for a fit
#' parafac or parafac2 model. For Parafac2, the diagnostic is calculated after
#' transforming the data.
#'
#' @param X Thee-way array with dim=c(I,J,K) or a four-way data array with dim=c(I,J,K,L). Can also input a list of two-way or three-way arrays (for Parafac2).
#' @param object Object of class "parafac" or class "parafac2".
#' @param divisor Divide by number of factors (default) or core sum of squares.
#'
#' @return CORCONDIA value
#' @export
#'
#' @examples
#' library(multiway)
#' ##########   EXAMPLE   ##########
#'
#' # create random data array with Parafac structure
#' set.seed(3)
#' mydim <- c(50,20,5)
#' nf <- 2
#' Amat <- matrix(rnorm(mydim[1]*nf),mydim[1],nf)
#' Bmat <- matrix(runif(mydim[2]*nf),mydim[2],nf)
#' Cmat <- matrix(runif(mydim[3]*nf),mydim[3],nf)
#' Xmat <- array(tcrossprod(Amat,krprod(Cmat,Bmat)),dim=mydim)
#' Emat <- array(rnorm(prod(mydim)),dim=mydim)
#' Emat <- nscale(Emat, 0, ssnew = sumsq(Xmat))   # SNR=1
#' X <- Xmat + Emat
#'
#' # fit Parafac model (1-4 factors)
#' pfac1 <- parafac(X,nfac=1,nstart=1)
#' pfac2 <- parafac(X,nfac=2,nstart=1)
#' pfac3 <- parafac(X,nfac=3,nstart=1)
#' pfac4 <- parafac(X,nfac=4,nstart=1)
#'
#' # check corcondia
#' corcondia(X, pfac1)
#' corcondia(X, pfac2)
#' corcondia(X, pfac3)
#' corcondia(X, pfac4)
corcondia = function (X, object, divisor = c("nfac", "core"))
{
  if(!any(is.na(X))){
    corcon = multiway::corcondia(X, object, divisor)
  }
  else{
    # Old approach
    # M = object$A %*% t(rTensor::khatri_rao(object$C, object$B))
    # reinflatedX = array(M, dim=dim(X))

    # New approach
    reinflatedX = reinflateBlock(object)

    # fill in NAs with imputed values
    newX = X
    newX[is.na(newX)] = reinflatedX[is.na(newX)]

    corcon = multiway::corcondia(newX, object, divisor)
  }

  return(corcon)
}
