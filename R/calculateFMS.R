#' Calculate Factor Match Score for all initialized models.
#'
#' @param models Output of [parafac()] using output="all".
#'
#' @return Vector containing FMS scores of all comparisons
#' @export
#'
#' @examples
#' A = array(rnorm(108*2), c(108, 2))
#' B = array(rnorm(100*2), c(100, 2))
#' C = array(rnorm(10*2), c(10, 2))
#' X = reinflateTensor(A, B, C)
#' models = parafac(X, 2, initialization="random", nstart=10, maxit=2, output="all")
#' calculateFMS(models)
calculateFMS = function(models){

  numModels = length(models)
  FMS_result = rep(0, numModels^2)
  FMSiterator = 1

  for(i in 1:(numModels-1)){
    for(j in (i+1):numModels){
      Fac1 = lapply(models[[i]]$Fac, as.matrix) # Make robust towards 1-component case
      Fac2 = lapply(models[[j]]$Fac, as.matrix) # Make robust towards 1-component case

      numComponents = ncol(Fac1[[1]])
      numComparisons = 0
      FMS = 0

      # Calculate FMS (see Li et al., 2024 for formula)
      # You need to iterate over all possible pairwise component comparisons to see which matches
      for(k in 1:numComponents){
        FMSproposal = list()
        for(l in 1:numComponents){
          total = 1

          for(m in 2:3){ # only check mode 2 and 3; these are not modified in size by bootstrapping
            vect1 = as.matrix(Fac1[[m]][,k])
            vect2 = as.matrix(Fac2[[m]][,l])

            term = abs(t(vect1) %*% vect2) / (norm(vect1,"F")*norm(vect2,"F"))
            total = total * term
          }
          FMSproposal[[l]] = total
        }
        FMS = FMS + max(unlist(FMSproposal))
        numComparisons = numComparisons + 1
      }

      if(numComparisons > 0){
        FMS_result[FMSiterator] = FMS / numComparisons
        FMSiterator = FMSiterator + 1
      }
      else{
        FMS_result[FMSiterator] = NA
        FMSiterator = FMSiterator + 1
      }
    }
  }

  return(FMS_result[FMS_result != 0])
}
