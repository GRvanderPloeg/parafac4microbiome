#' Export the PARAFAC model
#'
#' Currently only supports 3-mode models.
#'
#' @inheritParams plotPARAFACmodel
#' @param prefix Prefix file name
#' @param path Path to a folder
#' @param saveRDS Save an RDS of the PARAFAC model object? (true/false, default=TRUE)
#'
#' @return Saves mode1, mode2, mode3, input data, modelled data, and data as modelled per component in .csv files.
#' @export
#'
#' @examples
#' library(multiway)
#' library(dplyr)
#' library(ggplot2)
#' library(paramGUI)
#' library(pracma)
#' set.seed(0)
#'
#' # Make PARAFAC model
#' processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, centerMode=1, scaleMode=2)
#' model = parafac(processedFujita$data, nfac=2, nstart=1, verbose=FALSE)
#' \dontrun{
#' exportPARAFAC(model, processedFujita, prefix="Fujita")
#' }
#'
exportPARAFAC = function(model, dataset, prefix="PARAFACmodel", path="./", saveRDS=FALSE){
  stopifnot(methods::is(model, "parafac"))

  A = model$A
  B = model$B
  C = model$C
  numComponents = ncol(A)

  mode1 = cbind(A, dataset$mode1) %>% dplyr::as_tibble()
  mode2 = cbind(B, dataset$mode2) %>% dplyr::as_tibble()
  mode3 = cbind(C, dataset$mode3) %>% dplyr::as_tibble()
  input = dataset$data
  modelledData = reinflateTensor(model$Fac[[1]], model$Fac[[2]], model$Fac[[3]])

  components = list()
  for(f in 1:numComponents){
    fakeA = matrix(A[,f])
    fakeB = matrix(B[,f])
    fakeC = matrix(C[,f])
    fakeModel = list(fakeA, fakeB, fakeC)
    components[[f]] = reinflateTensor(fakeModel[[1]], fakeModel[[2]], fakeModel[[3]])
  }

  utils::write.table(mode1, paste0(path,prefix,"_mode1.csv"), sep=",", row.names=FALSE, col.names=TRUE)
  utils::write.table(mode2, paste0(path,prefix,"_mode2.csv"), sep=",", row.names=FALSE, col.names=TRUE)
  utils::write.table(mode3, paste0(path,prefix,"_mode3.csv"), sep=",", row.names=FALSE, col.names=TRUE)
  utils::write.table(input, paste0(path,prefix,"_input.csv"), sep=",", row.names=FALSE, col.names=TRUE)
  utils::write.table(modelledData, paste0(path,prefix,"_modelledData.csv"), sep=",", row.names=FALSE, col.names=TRUE)

  for(i in 1:length(components)){
    utils::write.table(components[[i]], paste0(path,prefix,"_component_",i,".csv"), sep=",", row.names=FALSE, col.names=TRUE)
  }

  if(saveRDS == TRUE){
    saveRDS(model, paste0(path,prefix,"_model.RDS"))
  }
}
