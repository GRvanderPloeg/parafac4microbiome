#' Process a multiway array of count data.
#'
#' @param dataset See [Fujita2023], [Shao2019] or [vanderPloeg2024].
#' @param sparsityThreshold Maximum sparsity for a feature to be selected
#' @param sparsityPerGroupFilter Consider groups when calculating sparsity
#' @param centerMode Center across mode
#' @param scaleMode Scale within mode
#'
#' @return CLRed, centered and scaled cube
#' @export
#'
#' @examples
#' processedCube = processDataCube(Fujita2023)
processDataCube = function(dataset, sparsityThreshold=1, sparsityPerGroupFilter=FALSE, centerMode=0, scaleMode=0){

  cube = dataset$data
  mode1 = dataset$mode1
  mode2 = dataset$mode2
  mode3 = dataset$mode3

  # Select features based on sparsity
  sparsity = calculateSparsity(cube)
  featureMask = sparsity <= sparsityThreshold

  # Calculate CLR
  cube_clr = multiwayCLR(cube)

  # Feature selection
  cube_filtered = cube_clr[,featureMask,]
  mode2_filtered = mode2[featureMask,]

  # Center
  if(centerMode != 0){
    cube_cnt = multiway::ncenter(cube_filtered, mode=centerMode)
  }
  else{
    cube_cnt = cube_filtered
  }

  # Scale - NOTE: THIS DOES NOT SCALE TO EXACTLY SD=1
  if(scaleMode != 0){
    cube_cnt_scl = multiway::nscale(cube_cnt, mode=scaleMode)
  }
  else{
    cube_cnt_scl = cube_cnt
  }

  return(list("data"=cube_cnt_scl, "mode1"=mode1, "mode2"=mode2_filtered, "mode3"=mode3))
}
