#' Plots Tucker Congruence Coefficients of randomly initialized models.
#'
#' @param models Models list output of [parafac()] using output="all".
#'
#' @return Plot of TCCs
#' @export
#'
#' @examples
#' processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, centerMode=1, scaleMode=2)
#' models = parafac(processedFujita$data, 3, nstart=10, output="all")
#' plotModelTCCs(models)
plotModelTCCs = function(models){

  numComponents = ncol(models[[1]]$Fac[[1]])
  numModels = length(models)
  numModes = length(models[[1]]$Fac)
  plotlist = list()
  plotIterator = 1

  if(numComponents == 1)
    return(NULL)

  for(i in 1:(numComponents-1)){
    for(j in (i+1):numComponents){

      # Calculate TCC
      overallTCC = rep(1, numModels)
      for(k in 1:numModes){
        TCC = rep(0, numModels)
        for(l in 1:numModels){
          TCC[l] = multiway::congru(models[[l]]$Fac[[k]][,i], models[[l]]$Fac[[k]][,j])
        }

        # Plot TCC
        title = paste0("Mode ", k, ", Components ", i, " vs. ", j)
        plotlist[[plotIterator]] = plotModelMetric(TCC, plottingMode="bar", ylabel="TCC", titleString=title) +
          ggplot2::theme(strip.background=ggplot2::element_blank(), strip.text.x = ggplot2::element_blank())
        plotIterator = plotIterator + 1
        overallTCC = overallTCC * TCC
      }

      # Plot overall TCC
      title = paste0("Overall TCC ", "Components ", i, " vs. ", j)
      plotlist[[plotIterator]] = plotModelMetric(overallTCC, plottingMode="bar", ylabel="TCC", titleString=title) +
        ggplot2::theme(strip.background=ggplot2::element_blank(), strip.text.x = ggplot2::element_blank())
      plotIterator = plotIterator + 1
    }
  }

  plot = ggpubr::ggarrange(plotlist=plotlist)
  return(plot)
}

# Ugly solution to namespace issues caused by dplyr
index <- NULL
value <- NULL
