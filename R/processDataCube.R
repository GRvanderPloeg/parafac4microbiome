#' Process a multi-way array of count data.
#'
#' @param dataset A longitudinal microbiome dataset, formatted as follows:
#' \describe{
#'   \item{data}{Array object of the data cube filled with counts}
#'   \item{mode1}{Dataframe with all the subject metadata, ordered the same as the rows in the data cube.}
#'   \item{mode2}{Taxonomic classification of the microbiota, ordered the same as the columns in the data cube.}
#'   \item{mode3}{Dataframe with the time metadata, ordered the same as the third dimension in the array.}
#' }
#' See [Fujita2023], [Shao2019] or [vanderPloeg2024] for more information.
#' @param sparsityThreshold Maximum sparsity for a feature to be selected (default=1, i.e. do not select features).
#' @param considerGroups Consider groups when calculating sparsity (default=FALSE).
#' @param groupVariable Column name in dataset$mode1 that should be used to consider groups (default="").
#' @param CLR Perform a centered log-ratio transformation of the count data (default=TRUE).
#' @param centerMode Mode to center across: 1=subjects,2=features,3=time (default 0, i.e. do not center). See [multiwayCenter()] for more information.
#' @param scaleMode Mode to scale within: 1=subjects,2=features,3=time (default 0, i.e. do not scale). See [multiwayScale()] for more information.
#'
#' @return CLRed, centered and scaled cube
#' @export
#'
#' @examples
#' processedCube = processDataCube(Fujita2023)
processDataCube = function(dataset, sparsityThreshold=1, considerGroups=FALSE, groupVariable="", CLR=TRUE, centerMode=0, scaleMode=0){

  cube = dataset$data
  mode1 = dataset$mode1
  mode2 = dataset$mode2
  mode3 = dataset$mode3

  # Select features based on sparsity
  if(considerGroups == TRUE & groupVariable %in% colnames(dataset$mode1)){
    sparsity = calculateSparsity(dataset, considerGroups=TRUE, groupVariable=groupVariable)
    featureMask = (colSums(sparsity <= sparsityThreshold) >= 1)
  }
  else{
    sparsity = calculateSparsity(dataset)
    featureMask = sparsity <= sparsityThreshold
  }

  # Calculate CLR
  if(CLR){
    cube_clr = multiwayCLR(cube)
  } else{
    cube_clr = cube
  }

  # Feature selection
  cube_filtered = cube_clr[,featureMask,]
  mode2_filtered = mode2[featureMask,]

  # Center
  if(centerMode != 0){
    cube_cnt = multiwayCenter(cube_filtered, mode=centerMode)
  }
  else{
    cube_cnt = cube_filtered
  }

  # Scale
  if(scaleMode != 0){
    cube_cnt_scl = multiwayScale(cube_cnt, mode=scaleMode)
  }
  else{
    cube_cnt_scl = cube_cnt
  }

  return(list("data"=cube_cnt_scl, "mode1"=mode1, "mode2"=mode2_filtered, "mode3"=mode3))
}
