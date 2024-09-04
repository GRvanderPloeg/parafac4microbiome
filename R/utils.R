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
      unflippedSSQ = apply(df-medianLoadings, 2, function(x){sumsqr(x,na.rm=TRUE)})
      flippedSSQ = apply((-1*df)-medianLoadings, 2, function(x){sumsqr(x,na.rm=TRUE)})
      evidence[j,] = flippedSSQ <= unflippedSSQ
    }
    flipEvidencePerComponent[[i]] = evidence
  }

  # Flip relevant loadings
  for(i in 1:numModels){
    Fac = models[[i]]$Fac

    for(j in 1:numComponents){
      if(sum(flipEvidencePerComponent[[j]][,i]) == 2){
        for(k in 1:numModes){
          if(flipEvidencePerComponent[[j]][k,i] == 1){
            Fac[[k]][,j] = -1 * Fac[[k]][,j]
          }
        }
      }
    }
    models[[i]]$Fac = Fac
  }

  return(models)
}

#' Create a tensor out of a set of matrices similar to a component model.
#'
#' @param A I x N matrix corresponding to loadings in the first mode for N components.
#' @param B J x N matrix corresponding to loadings in the second mode for N components.
#' @param C K x N matrix corresponding to loadings in the third mode for N components.
#' @param returnAsTensor Boolean return as [rTensor] S4 tensor object (default FALSE).
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

#' Sum-of-squares calculation
#'
#' @param X Either a list containing matrices, or a matrix of values.
#' @param na.rm Remove NAs from calculation (default FALSE).
#'
#' @return sum-of-squares of the object
#' @export
#'
#' @examples
#' X = array(rnorm(108*100*10), c(108,100,10))
#' ssq = sumsqr(X)
sumsqr = function(X, na.rm=FALSE){

  if(methods::is(X, "list")){
    result = sum(sapply(X, sumsqr, na.rm=na.rm)) # recursive to iterate over list
  } else{
    result = sum(X^2, na.rm=na.rm)
  }

  return(result)
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
#' @param Fac Fac object output of a PARAFAC model
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
