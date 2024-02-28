#' Process a multiway array of count data.
#'
#' @param cube Multiway array (currently only supports 3-way)
#' @param sparsityThreshold Maximum sparsity for a feature to be selected
#' @param sparsityPerGroupFilter Consider groups when calculating sparsity
#' @param centerMode Center across mode
#' @param scaleMode Scale within mode
#'
#' @return CLRed, centered and scaled cube
#' @export
#'
#' @examples
#' processedCube = processDataCube(Fujita2023$data)
processDataCube = function(cube, sparsityThreshold=1, sparsityPerGroupFilter=FALSE, centerMode=0, scaleMode=0){

  # Select features based on sparsity
  sparsity = calculateSparsity(cube)
  featureMask = sparsity <= sparsityThreshold

  # Calculate CLR
  cube_clr = multiwayCLR(cube)

  # Feature selection
  cube_filtered = cube_clr[,featureMask,]

  # Center
  cube_cnt = multiway::ncenter(cube_filtered, mode=centerMode)

  # Scale - NOTE: THIS DOES NOT SCALE TO EXACTLY SD=1
  cube_cnt_scl = multiway::nscale(cube_cnt, mode=scaleMode)

  return(cube_cnt_scl)
}
