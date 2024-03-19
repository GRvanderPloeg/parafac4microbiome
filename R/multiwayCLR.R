#' Perform a centered log-ratio transform over a multi-way array
#'
#' Note: Propagates NAs corresponding to missing samples.
#'
#' @param cube Multi-way array of counts
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
      sample = cube_pseudo[i,,k]

      if(all(is.na(sample))){ # propagate NAs corresponding to missing samples
        cube_clr[i,,k] = sample
      }
      else{
        cube_clr[i,,k] = compositions::clr(sample)
      }
    }
  }

  return(cube_clr)
}
