#' Import PARAFAC model
#'
#' @inheritParams exportPARAFAC
#' @param numComponents Number of components in the model
#' @param sep Separator character for the input files (default=",")
#' @param header Expect headers in the csv files (default=TRUE)
#' @param loadRDS Load a previously saved RDS object of the model (default=TRUE)
#'
#' @return List of: the PARAFAC model, the dataset used, the data as modelled
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
#' exportPARAFAC(model, processedFujita, prefix="Fujita_")
#' importedModel = importPARAFAC(path="./", prefix="Fujita_", numComponents=2)
#' }
#'
importPARAFAC = function(path, prefix, numComponents, sep=",", loadRDS=TRUE, header=TRUE){
  mode1 = utils::read.csv(paste0(path,prefix,"_mode1.csv"), sep=sep, header=header) %>% dplyr::as_tibble()
  mode2 = utils::read.csv(paste0(path,prefix,"_mode2.csv"), sep=sep, header=header) %>% dplyr::as_tibble()
  mode3 = utils::read.csv(paste0(path,prefix,"_mode3.csv"), sep=sep, header=header) %>% dplyr::as_tibble()
  input = utils::read.csv(paste0(path,prefix,"_input.csv"), sep=sep, header=header) %>% dplyr::as_tibble()
  modelledData = utils::read.csv(paste0(path,prefix,"_model.csv"), sep=sep, header=header) %>% dplyr::as_tibble()

  components = list()
  for(i in 1:length(numComponents)){
    components[[i]] = utils::read.csv(paste0(path,prefix,"_component_",i,".csv"), sep=sep, header=header) %>% dplyr::as_tibble()
  }

  dataset = list("data"=input, "mode1"=mode1[,(numComponents+1):ncol(mode1)], "mode2"=mode2[,(numComponents+1):ncol(mode2)], "mode3"=mode3[,(numComponents+1):ncol(mode3)])

  if(loadRDS){
    model = readRDS(paste0(path,prefix,"_model.RDS"))
  }
  else{
    model = list("A"=mode1[,1:numComponents], "B"=mode2[,1:numComponents], "C"=mode3[,1:numComponents])
  }

  output = list("model"=model, "dataset"=dataset, "reconstructedData"=modelledData, "reconstructedPerComponent"=components)
}
