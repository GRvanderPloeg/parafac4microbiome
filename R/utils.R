#' Sign flip the loadings of many randomly initialized models to make consistent overview plots.
#'
#' @param models Output of [parafac].
#' @param X Input dataset of parafac modelling procedure.
#'
#' @return models with sign flipped components where applicable.
#' @export
#'
#' @examples
#' A = array(rnorm(108*2), c(108,2))
#' B = array(rnorm(100*2), c(100,2))
#' C = array(rnorm(10*2), c(10,2))
#' X = reinflateTensor(A, B, C)
#' models = parafac(X, 2, nstart=10, output="all", sortComponents=TRUE)
#' flippedModels = flipLoadings(models, X)
flipLoadings = function(models, X){

  # Recognize one-model and multi-model case
  if(!is.null(models$Fac)){
    model = models
    models = list()
    models[[1]] = model
  }

  numModels = length(models)
  numModes = length(models[[1]]$Fac)
  numComponents = ncol(models[[1]]$Fac[[1]])

  # Make sure all models have sorted components for consistency
  for(i in 1:numModels){
    models[[i]]$Fac = sortComponents(models[[i]]$Fac, X)
  }

  # Check for flipping evidence per mode and component
  flipEvidencePerComponent = list()
  for(i in 1:numComponents){
    evidence = array(0L, c(numModes, numModels))

    for(j in 1:numModes){

      # Collect all loadings corresponding to one mode and one component
      df = simplify2array(lapply(models, function(model){model$Fac[[j]][,i]}))

      # Arbitrarily define the "correct loadings" as the median of all folds
      medianLoadings = apply(df, 1, function(x){stats::median(x, na.rm=TRUE)})

      # Check if SSQ goes down by flipping sign
      unflippedSSQ = apply(df-medianLoadings, 2, function(x){multiway::sumsq(x,na.rm=TRUE)})
      flippedSSQ = apply((-1*df)-medianLoadings, 2, function(x){multiway::sumsq(x,na.rm=TRUE)})
      evidence[j,] = flippedSSQ <= unflippedSSQ
    }
    flipEvidencePerComponent[[i]] = evidence
  }

  # Flip relevant loadings
  for(i in 1:numModels){
    Fac = models[[i]]$Fac

    for(j in 1:numComponents){
      if(sum(flipEvidencePerComponent[[j]][,i]) >= 2){
        for(k in numModes:1){
          numFlipped = 0 # if all modes need to be flipped, flip maximally 2
          if((flipEvidencePerComponent[[j]][k,i] == 1) & (numFlipped < 2)){
            Fac[[k]][,j] = -1 * Fac[[k]][,j]
            numFlipped = numFlipped + 1
          }
        }
      }
    }
    models[[i]]$Fac = Fac
  }

  if(length(models) > 1){
    return(models)
  } else{
    return(models[[1]]) # deal with one-model case
  }
}

#' Create a tensor out of a set of matrices similar to a component model.
#'
#' @param A I x N matrix corresponding to loadings in the first mode for N components.
#' @param B J x N matrix corresponding to loadings in the second mode for N components.
#' @param C K x N matrix corresponding to loadings in the third mode for N components.
#' @param returnAsTensor Boolean return as rTensor S4 tensor object (default FALSE).
#'
#' @return M, an I x J x K tensor.
#' @export
#'
#' @examples
#' A = rnorm(108)
#' B = rnorm(100)
#' C = rnorm(10)
#' M = reinflateTensor(A,B,C)
reinflateTensor = function(A, B, C, returnAsTensor=FALSE){

  A = as.matrix(A)
  B = as.matrix(B)
  C = as.matrix(C)
  reinflatedTensor = array(tcrossprod(A, multiway::krprod(C, B)), c(nrow(A),nrow(B),nrow(C)))

  if(returnAsTensor == TRUE){
    reinflatedTensor = rTensor::as.tensor(reinflatedTensor)
  }
  return(reinflatedTensor)
}

#' Calculate Xhat from a model Fac object
#'
#' @param Fac Fac object from parafac
#' @param X Input data X
#' @param returnAsTensor Boolean to return Xhat as rTensor tensor (TRUE) or matrix (default, FALSE).
#'
#' @return Xhat
#' @export
#'
#' @examples
#' processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, centerMode=1, scaleMode=2)
#' model = parafac(processedFujita$data, nfac=1, nstart=1, verbose=FALSE)
#' Xhat = reinflateFac(model$Fac, processedFujita$data)
reinflateFac = function(Fac, X, returnAsTensor=FALSE){
  Fac = lapply(Fac, as.matrix) # Cast to matrix for correct indexation in the one-component case.
  numModes = length(dim(X))
  numComponents = ncol(Fac[[1]])

  # Check for lambdas i.e. kruskal tensors
  kruskal = FALSE
  if(length(Fac) > numModes){
    kruskal = TRUE
  }

  # Calculate reinflated data
  Xhat = array(0L, dim(X))
  for(i in 1:numComponents){
    lambda = ifelse(kruskal, Fac[[numModes+1]][i], 1) # Check for ACMTF model lambdas, otherwise lambda=1
    Xhat = Xhat + lambda * reinflateTensor(Fac[[1]][,i], Fac[[2]][,i], Fac[[3]][,i])
  }

  if(returnAsTensor == TRUE){
    Xhat = rTensor::as.tensor(Xhat)
  }
  return(Xhat)
}

#' Calculate the variance explained of a PARAFAC model, per component
#'
#' @param Fac Fac object output of a model
#' @param X Input dataset
#'
#' @return Vector of scalars of the percentage of variation explained per component
#' @export
#'
#' @examples
#' X = array(rnorm(108*100*10), c(108,100,10))
#' model = parafac(X, 2)
#' calcVarExpPerComponent(model$Fac, X)
calcVarExpPerComponent = function(Fac, X){
  if(methods::is(X, "Tensor")){
    X = X@data
  }

  Fac = lapply(Fac, as.matrix) # protection from the 1-component case
  numComponents = ncol(Fac[[1]])
  numModes = length(Fac)

  varExpsPerComp = rep(0, numComponents)
  for(i in 1:numComponents){
    compFac = list()
    for(j in 1:numModes){
      compFac[[j]] = Fac[[j]][,i]
    }
    varExpsPerComp[i] = calculateVarExp(compFac, X) * 100
  }

  return(varExpsPerComp)
}

#' Sort PARAFAC components based on variance explained per component.
#'
#' @param Fac Fac object output of a [parafac] model
#' @param X Input data
#'
#' @return Fac object of sorted components
#' @export
#'
#' @examples
#' X = array(rnorm(108*100*10), c(108,100,10))
#' model = parafac(X, 2)
#' sortedFac = sortComponents(model$Fac, X)
sortComponents = function(Fac, X){
  numModes = length(Fac)
  varExpsPerComp = calcVarExpPerComponent(Fac, X)
  sorting = sort(varExpsPerComp, decreasing=TRUE, index.return=TRUE)$ix

  for(i in 1:numModes){
    Fac[[i]] = Fac[[i]][,sorting]
  }

  # Put Fac in matrix form to avoid problems with 1-component case
  Fac = lapply(Fac, as.matrix)
  return(Fac)
}

#' Vectorize Fac object
#'
#' @param Fac Fac object output of [parafac].
#'
#' @return Vectorized Fac object
#' @export
#'
#' @examples
#' set.seed(123)
#' A = array(rnorm(108*2), c(108, 2))
#' B = array(rnorm(100*2), c(100, 2))
#' C = array(rnorm(10*2), c(10, 2))

#' Fac = list(A, B, C)
#' v = fac_to_vect(Fac)
fac_to_vect = function(Fac){
  return(unlist(Fac))
}

#' Convert vectorized output of PARAFAC to a Fac list object with all loadings per mode.
#'
#' @param vect Vectorized output of PARAFAC modelling
#' @param X Input data
#' @param sortComponents Sort the order of the components by variation explained (default FALSE).
#'
#' @return Fac: list object with all loadings in all components per mode, ordered the same way as Z$modes.
#' @export
#'
#' @examples
#' set.seed(123)
#' A = array(rnorm(108*2), c(108, 2))
#' B = array(rnorm(100*2), c(100, 2))
#' C = array(rnorm(10*2), c(10, 2))
#'
#' X = reinflateTensor(A, B, C)
#' result = initializePARAFAC(X, 2, initialization="random", output="vect")
#' Fac = vect_to_fac(result, X)
vect_to_fac = function(vect, X, sortComponents=FALSE){

  numModes = length(dim(X))
  numComponents = length(vect) / sum(dim(X))
  sizes = dim(X)

  Fac = list()
  startIdx = 1
  for(i in 1:numModes){
    Fac[[i]] = array(0L, c(sizes[i], numComponents))

    for(r in 1:numComponents){
      endIdx = startIdx + sizes[i] - 1
      Fac[[i]][,r] = vect[startIdx:endIdx]
      startIdx = endIdx + 1
    }
  }

  # If there are values leftover, you must have an ACMTF model
  ACMTFcase = FALSE
  if(endIdx < length(vect)){
    ACMTFcase = TRUE
  }

  # If you have an ACMTF model, add the remaining values as lambdas
  if(ACMTFcase){
    Fac[[numModes+1]] = array(0L, c(1, numComponents))
    for(r in 1:numComponents){
      endIdx = startIdx
      Fac[[numModes+1]][,r] = vect[startIdx:endIdx]
      startIdx = endIdx + 1
    }
  }

  if(sortComponents == TRUE){
    Fac = sortComponents(Fac, X)
  }

  return(Fac)
}

ignore_unused_imports <- function() {
  rlang::is_installed
}
