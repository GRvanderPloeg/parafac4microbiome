#' Scale a multi-way array
#'
#' Note: this function exists because [multiway::nscale()] cannot handle NAs.
#'
#' @param cube Multi-way array
#' @param mode Mode to scale within: 1=subjects,2=features,3=time (default 2).
#'
#' @return Scaled multi-way array
#' @export
#'
#' @examples
#' cube_scl = multiwayCenter(Fujita2023$data)
multiwayScale = function(cube, mode=2){
  I = dim(cube)[1]
  J = dim(cube)[2]
  K = dim(cube)[3]

  cube_scl = array(0L, dim=c(I,J,K))

  if(mode == 1){      # Scale within subject mode
    for(i in 1:I){
        cube_scl[i,,] = cube[i,,] / stats::sd(cube[i,,], na.rm=TRUE)
    }
  }
  else if(mode == 2){ # Scale within feature mode
    for(j in 1:J){
      cube_scl[,j,] = cube[,j,] / stats::sd(cube[,j,], na.rm=TRUE)
    }
  }
  else if(mode == 3){ # Scale within "time" mode
    for(k in 1:K){
        cube_scl[,,k] = cube[,,k] / stats::sd(cube[,,k], na.rm=TRUE)
    }
  }

  return(cube_scl)
}
