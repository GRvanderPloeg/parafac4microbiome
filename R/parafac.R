#' Parallel Factor Analysis-1
#'
#' Fits Richard A. Harshman's Parallel Factors (Parafac) model to 3-way or 4-way data arrays. Parameters are estimated via alternating least squares with optional constraints.
#'
#' @param X Three-way data array with dim=c(I,J,K) or four-way data array with dim=c(I,J,K,L). Missing data are allowed (see Note).
#' @param nfac Number of factors.
#' @param nstart Number of random starts.
#' @param const Character vector of length 3 or 4 giving the constraints for each mode (defaults to unconstrained). See [CMLS::const] for the 24 available options.
#' @param control List of parameters controlling options for smoothness constraints. This is passed to [multiway::const.control], which describes the available options.
#' @param Afixed Used to fit model with fixed Mode A weights.
#' @param Bfixed Used to fit model with fixed Mode B weights.
#' @param Cfixed Used to fit model with fixed Mode C weights.
#' @param Dfixed Used to fit model with fixed Mode D weights.
#' @param Astart Starting Mode A weights. Default uses random weights.
#' @param Bstart Starting Mode B weights. Default uses random weights.
#' @param Cstart Starting Mode C weights. Default uses random weights.
#' @param Dstart Starting Mode D weights. Default uses random weights.
#' @param Astruc Structure constraints for Mode A weights. See Note.
#' @param Bstruc Structure constraints for Mode B weights. See Note.
#' @param Cstruc Structure constraints for Mode C weights. See Note.
#' @param Dstruc Structure constraints for Mode D weights. See Note.
#' @param Amodes Mode ranges for Mode A weights (for unimodality constraints). See Note.
#' @param Bmodes Mode ranges for Mode B weights (for unimodality constraints). See Note.
#' @param Cmodes Mode ranges for Mode C weights (for unimodality constraints). See Note.
#' @param Dmodes Mode ranges for Mode D weights (for unimodality constraints). See Note.
#' @param maxit Maximum number of iterations.
#' @param ctol Convergence tolerance (R^2 change).
#' @param parallel Logical indicating if [parallel::parLapply] should be used. See Examples.
#' @param cl 	Cluster created by [parallel::makeCluster]. Only used when parallel=TRUE.
#' @param output Output the best solution (default) or output all nstart solutions.
#' @param verbose If TRUE, fitting progress is printed via [utils::txtProgressBar]. Ignored if parallel=TRUE.
#' @param backfit Should backfitting algorithm be used for [CMLS::cmls]?
#'
#' @return If output = "best", returns an object of class "parafac" with the following elements:
#'#' \describe{
#'   \item{A}{Mode A weight matrix.}
#'   \item{B}{Mode B weight matrix.}
#'   \item{C}{Mode C weight matrix.}
#'   \item{D}{Mode D weight matrix.}
#'   \item{SSE}{Sum of Squared Errors.}
#'   \item{Rsq}{R-squared value.}
#'   \item{GCV}{Generalized Cross-Validation.}
#'   \item{edf}{Effective degrees of freedom.}
#'   \item{iter}{Number of iterations.}
#'   \item{cflag}{Convergence flag. See Note.}
#'   \item{const}{See argument const.}
#'   \item{control}{See argument control.}
#'   \item{fixed}{Logical vector indicating whether 'fixed' weights were used for each mode.}
#'   \item{struc}{Logical vector indicating whether 'struc' constraints were used for each mode.}
#' }
#' Otherwise returns a list of length nstart where each element is an object of class "parafac".
#'
#' @note
#' Missing data should be specified as NA values in the input X. The missing data are randomly initialized and then iteratively imputed as a part of the algorithm.
#'
#' Structure constraints should be specified with a matrix of logicals (TRUE/FALSE), such that FALSE elements indicate a weight should be constrained to be zero. Default uses unstructured weights, i.e., a matrix of all TRUE values.
#'
#' When using unimodal constraints, the *modes inputs can be used to specify the mode search range for each factor. These inputs should be matrices with dimension c(2,nfac) where the first row gives the minimum mode value and the second row gives the maximum mode value (with respect to the indicies of the corresponding weight matrix).
#'
#' Output cflag gives convergence information: cflag = 0 if algorithm converged normally, cflag = 1 if maximum iteration limit was reached before convergence, and cflag = 2 if algorithm terminated abnormally due to a problem with the constraints.
#' @export
#'
#' @examples
#' library(multiway)
#' #' ##########   3-way example   ##########
#'
#' # create random data array with Parafac structure
#' set.seed(3)
#' mydim <- c(50, 20, 5)
#' nf <- 3
#' Amat <- matrix(rnorm(mydim[1]*nf), nrow = mydim[1], ncol = nf)
#' Bmat <- matrix(runif(mydim[2]*nf), nrow = mydim[2], ncol = nf)
#' Cmat <- matrix(runif(mydim[3]*nf), nrow = mydim[3], ncol = nf)
#' Xmat <- tcrossprod(Amat, krprod(Cmat, Bmat))
#' Xmat <- array(Xmat, dim = mydim)
#' Emat <- array(rnorm(prod(mydim)), dim = mydim)
#' Emat <- nscale(Emat, 0, ssnew = sumsq(Xmat))   # SNR = 1
#' X <- Xmat + Emat
#'
#' # fit Parafac model (unconstrained)
#' pfac <- parafac(X, nfac = nf, nstart = 1)
#' pfac
#'
#' # fit Parafac model (non-negativity on Modes B and C)
#' pfacNN <- parafac(X, nfac = nf, nstart = 1,
#'             const = c("uncons", "nonneg", "nonneg"))
#' pfacNN
#'
#' # check solution
#' Xhat <- fitted(pfac)
#' sum((Xmat - Xhat)^2) / prod(mydim)
#'
#' # reorder and resign factors
#' pfac$B[1:4,]
#' pfac <- reorder(pfac, c(3,1,2))
#' pfac$B[1:4,]
#' pfac <- resign(pfac, mode="B")
#' pfac$B[1:4,]
#' Xhat <- fitted(pfac)
#' sum((Xmat - Xhat)^2) / prod(mydim)
#'
#' # rescale factors
#' colSums(pfac$B^2)
#' colSums(pfac$C^2)
#' pfac <- rescale(pfac, mode = "C", absorb = "B")
#' colSums(pfac$B^2)
#' colSums(pfac$C^2)
#' Xhat <- fitted(pfac)
#' sum((Xmat - Xhat)^2) / prod(mydim)
#'
#'
#' ##########   4-way example   ##########
#'
#' # create random data array with Parafac structure
#' set.seed(4)
#' mydim <- c(30,10,8,10)
#' nf <- 4
#' aseq <- seq(-3, 3, length.out = mydim[1])
#' Amat <- cbind(dnorm(aseq), dchisq(aseq+3.1, df=3),
#'       dt(aseq-2, df=4), dgamma(aseq+3.1, shape=3, rate=1))
#' Bmat <- svd(matrix(runif(mydim[2]*nf), nrow = mydim[2], ncol = nf), nv = 0)$u
#' Cmat <- matrix(runif(mydim[3]*nf), nrow = mydim[3], ncol = nf)
#' Cstruc <- Cmat > 0.5
#' Cmat <- Cmat * Cstruc
#' Dmat <- matrix(runif(mydim[4]*nf), nrow = mydim[4], ncol = nf)
#' Xmat <- tcrossprod(Amat, krprod(Dmat, krprod(Cmat, Bmat)))
#' Xmat <- array(Xmat, dim = mydim)
#' Emat <- array(rnorm(prod(mydim)), dim = mydim)
#' Emat <- nscale(Emat, 0, ssnew = sumsq(Xmat))   # SNR = 1
#' X <- Xmat + Emat
#'
#' # fit Parafac model (unimodal and smooth A, orthogonal B,
#' #                    non-negative and structured C, non-negative D)
#' pfac <- parafac(X, nfac = nf, nstart = 1, Cstruc = Cstruc,
#'             const = c("unismo", "orthog", "nonneg", "nonneg"))
#' pfac
#'
#' # check solution
#' Xhat <- fitted(pfac)
#' sum((Xmat - Xhat)^2) / prod(mydim)
#' congru(Amat, pfac$A)
#' crossprod(pfac$B)
#' pfac$C
#' Cstruc
#'
parafac = function (X, nfac, nstart = 10, const = NULL, control = NULL,
                    Afixed = NULL, Bfixed = NULL, Cfixed = NULL, Dfixed = NULL,
                    Astart = NULL, Bstart = NULL, Cstart = NULL, Dstart = NULL,
                    Astruc = NULL, Bstruc = NULL, Cstruc = NULL, Dstruc = NULL,
                    Amodes = NULL, Bmodes = NULL, Cmodes = NULL, Dmodes = NULL,
                    maxit = 500, ctol = 1e-04, parallel = FALSE, cl = NULL, output = c("best",
                                                                                       "all"), verbose = TRUE, backfit = FALSE)
{
  models = multiway::parafac(X, nfac, nstart, const, control,
                            Afixed, Bfixed, Cfixed, Dfixed,
                            Astart, Bstart, Cstart, Dstart,
                            Astruc, Bstruc, Cstruc, Dstruc,
                            Amodes, Bmodes, Cmodes, Dmodes,
                            maxit, ctol, parallel, cl, output, verbose, backfit)

  # Put all the variation into mode A to make the models equal to Rasmus Bro's implementation in Matlab
  if(nstart > 1){
    newModels = list()
    for(i in 1:nstart){
      newModel = models[[i]]
      newModel = multiway::rescale(newModel, mode="B", newscale=sqrt(1/nrow(newModel$B)), absorb="A")
      newModel = multiway::rescale(newModel, mode="C", newscale=sqrt(1/nrow(newModel$C)), absorb="A")

      if(length(dim(X))==4){
        newModel = multiway::rescale(newModel, mode="D", newscale=sqrt(1/nrow(newModel$D)), absorb="A")
      }
      newModels[[i]] = newModel
    }
    return(newModels)
  }
  else{
    model = models
    model = multiway::rescale(model, mode="B", newscale=sqrt(1/nrow(model$B)), absorb="A")
    model = multiway::rescale(model, mode="C", newscale=sqrt(1/nrow(model$C)), absorb="A")

    if(length(dim(X))==4){
      model = multiway::rescale(model, mode="D", newscale=sqrt(1/nrow(model$D)), absorb="A")
    }
    return(model)
  }
}
