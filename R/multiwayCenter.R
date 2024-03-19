#' Center a multi-way array
#'
#' Note: this function exists because [multiway::ncenter()] cannot handle NAs.
#'
#' @param cube Multi-way array
#' @param mode Mode to center across: 1=subjects,2=features,3=time (default 1).
#'
#' @return Centered multi-way array
#' @export
#'
#' @examples
#' cube_cnt = multiwayCenter(Fujita2023$data)
multiwayCenter = function(cube, mode=1){
  I = dim(cube)[1]
  J = dim(cube)[2]
  K = dim(cube)[3]

  cube_cnt = array(0L, dim=c(I,J,K))

  if(mode == 1){      # Center across subject mode
    for(j in 1:J){
      for(k in 1:K){
        cube_cnt[,j,k] = cube[,j,k] - mean(cube[,j,k], na.rm=TRUE)
      }
    }
  }
  else if(mode == 2){ # Center across feature mode
    for(i in 1:I){
      for(k in 1:K){
        cube_cnt[i,,k] = cube[i,,k] - mean(cube[i,,k], na.rm=TRUE)
      }
    }
  }
  else if(mode == 3){ # Center across "time" mode
    for(i in 1:I){
      for(j in 1:J){
        cube_cnt[i,j,] = cube[i,j,] - mean(cube[i,j,], na.rm=TRUE)
      }
    }
  }

  return(cube_cnt)
}
