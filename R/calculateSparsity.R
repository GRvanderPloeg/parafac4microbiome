#' Calculate sparsity across the feature mode of a multiway array
#'
#' @param dataset See [Fujita2023], [Shao2019] or [vanderPloeg2024].
#' @param considerGroups Consider subject groups in calculating sparsity (default FALSE)
#' @param groupVariable What column name of mode 1 should be used to consider groups? (default "")
#'
#' @return Vector of sparsity fractions (N x J) where N is the number of groups and J is the number of features.
#' @export
#'
#' @examples
#' # No groups
#' sparsity = calculateSparsity(Fujita2023)
#' length(sparsity)
#' hist(sparsity)
#'
#' # Consider groups
#' colnames(Shao2019$mode1)
#' sparsity = calculateSparsity(Shao2019, considerGroups=TRUE, groupVariable="Delivery_mode")
#' dim(sparsity)
#' hist(sparsity[1,])
#' hist(sparsity[2,])
#'
calculateSparsity = function(dataset, considerGroups=FALSE, groupVariable=""){
  cube = dataset$data
  I = dim(cube)[1]
  J = dim(cube)[2]
  K = dim(cube)[3]

  if(considerGroups == TRUE & groupVariable == ""){
    warning("When setting considerGroups to TRUE, please also specify a groupVariable.")
    return(0)
  } else if(considerGroups == FALSE & groupVariable != ""){
    considerGroups = TRUE
  }

  if(considerGroups == TRUE & groupVariable %in% colnames(dataset$mode1)){
    groups = dataset$mode1[,groupVariable] %>% dplyr::pull() %>% unique()
    numGroups = length(groups)
    sparsity = matrix(0L, nrow=numGroups, ncol=J)

    for(i in 1:numGroups){
      group = groups[i]
      sampleMask = dataset$mode1[,groupVariable] == group

      for(j in 1:J){
        v = c(cube[sampleMask,j,]) # vectorized values of feature j in group i
        v = v[!is.na(v)] # do not consider NAs
        sparsity[i,j] = sum(v==0) / length(v)
      }
    }
  }
  else{
    sparsity = 1:J
    for(j in 1:J){
      v = c(cube[,j,]) # vectorized values of feature j
      v = v[!is.na(v)] # do not consider NAs
      sparsity[j] = sum(v==0) / length(v)
    }
  }

  return(sparsity)
}
