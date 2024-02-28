#' Perform a centered log-ratio transform over a multiway array
#'
#' @param cube Multiway array of counts
#'
#' @return CLRed cube
#' @export
#'
#' @examples
#' cubeCLR = multiwayCLR(Fujita2023$data)
multiwayCLR = function(cube){
  I = dim(cube)[1]
  J = dim(cube)[2]
  K = dim(cube)[3]

  cube_pseudo = cube + 1
  cube_clr = array(NA, dim=c(I,J,K))

  for(i in 1:I){
    for(k in 1:K){
      cube_clr[i,,k] = compositions::clr(cube_pseudo[i,,k])
    }
  }

  return(cube_clr)
}
