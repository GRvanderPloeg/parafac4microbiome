calculateCORCONDIA = function(X, model){
  stopifnot(class(model) == "parafac")

  numComponents = ncol(model$A)
  numModes = length(model$const)

  # Initialize perfect core array with superdiagonal of ones and zeros elsewhere
  G = array(0L, dim=rep(numComponents,numModes))
  for(i in 1:utils::tail(dim(G),n=1)){
    G[,,i] = diag(numComponents)
  }

  # Find the core array fitting the PARAFAC model
  t3core = multiway::tucker(X, rep(numComponents,numModes), nstart=1, Afixed=model$A, Bfixed=model$B, Cfixed=model$C, verbose=FALSE)$G

  if(numComponents==1){
    t3core_scaled = t3core / t3core
  } else{
    t3core_scaled = t3core / stats::sd(t3core)
  }

  # Calculate CORCONDIA score
  CORCONDIA = 0
  for(d in 1:numComponents){
    for(e in 1:numComponents){
      for(f in 1:numComponents){
        CORCONDIA = CORCONDIA + (t3core_scaled[d,e,f] - G[d,e,f])^2
      }
    }
  }
  CORCONDIA = 100 * (1 - (CORCONDIA / sum(G^2)))
  return(CORCONDIA)
}
