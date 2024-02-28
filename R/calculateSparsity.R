#' Calculate sparsity across the feature mode of a multiway array
#'
#' @param cube Multiway array of counts
#'
#' @return Vector of sparsity fractions (1 x J) where J is the number of features.
#' @export
#'
#' @examples
#' sparsity = calculateSparsity(Fujita2023$data)
calculateSparsity = function(cube){
  I = dim(cube)[1]
  J = dim(cube)[2]
  K = dim(cube)[3]

  sparsity = 1:J
  for(j in 1:J){
    v = c(cube[,j,]) # vectorized values of feature j
    sparsity[j] = sum(v==0)/length(v)
  }

  return(sparsity)
}
