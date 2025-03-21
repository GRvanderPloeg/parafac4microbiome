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
corcondia = function(X, Fac) {
  DimX = dim(X)
  X = array(X, c(DimX[1], prod(DimX[-1])))
  numComponents = ncol(Fac[[1]])
  numModes = length(Fac)

  # Scale all loadings to same magnitude
  magn = rep(1, numComponents)
  for (i in 1:numModes) {
    L = Fac[[i]]
    for (f in 1:numComponents) {
      magn[f] = magn[f] * norm(L[,f], "2")
      L[, f] = L[,f] / norm(L[,f], "2")
    }
    Fac[[i]] = L
  }

  # Magn holds the singular value of each component.
  # Scale each loading vector by the cubic root (if three-way), so
  # all loadings of a component have the same variance.
  magn = magn^(1 / numModes)
  for (i in 1:numModes) {
    L = Fac[[i]]
    for (f in 1:numComponents) {
      L[,f] = L[,f] * magn[f]
    }
    Fac[[i]] = L
  }

  # Make a diagonal array holding the magnitudes
  Ident = array(0, dim = rep(numComponents,3))
  for(k in 1:numComponents){
    Ident[k,k,k] = 1
  }

  if(numComponents > 1){
    Ident = rTensor::k_unfold(rTensor::as.tensor(Ident), 1)@data
  }

  # Make matrix of Kronecker product of all loadings,
  # except the large; Z = kron(C,B,...)
  newFac = list()
  newFacNo = c()
  for(i in numModes:1){
    Z = Fac[[i]]

    # Check if Z is full rank, or adjust the core and use less columns
    rankZ = qr(Fac[[i]])$rank
    # if(rankZ < numComponents){
    #   QRdecomp = qr(Z);
    #   r = qr.R(temp)
    #   q = qr.Q(temp)
    #   Ident = r * Ident
    #   Z = q
    # }

    newFac[[i]] = Z
    newFacNo = c(rankZ, newFacNo)
  }

  # Calculate G
  vecX = c(X)
  vecX[is.na(vecX)] = 0
  G = c(pracma::pinv(pracma::kron(pracma::kron(Fac[[3]], Fac[[2]]), Fac[[1]])) %*% vecX) # eq 5 bro kiers 2003

  Ident = c(Ident)
  ssG = sum(G^2)

  result = 100 * (1 - sum((G - Ident)^2)/ssG)
  return(result)
}
