#' Plot model stability check output.
#'
#' @inheritParams plotPARAFACmodel
#' @inheritParams processDataCube
#' @param modelStabilityOutput Outout of the [modelStabilityCheck()] function.
#' @return Plot
#' @export
#'
#' @examples
#' library(dplyr)
#' library(ggplot2)
#' library(ggpubr)
#'
#' processedFujita = processDataCube(Fujita2023,
#' sparsityThreshold=0.99,
#' centerMode=1,
#' scaleMode=2)
#'
#' modelStability = modelStabilityCheck(processedFujita$data,
#' processedFujita$sampleMetadata,
#' numComponents=3)
#'
#' plotModelStability(modelStability,
#'  dataset=processedFujita,
#'  colourCols=c("","Phylum", ""),
#'  legendTitles=c("","Phylum",""),
#'  xLabels=c("Replicate", "Feature index", "Time index"),
#'  legendColNums=c(0,3,0),
#'  arrangeModes=c(FALSE,TRUE,FALSE),
#'  overallTitle="Fujita2023")
#'
plotModelStability = function(modelStabilityOutput, dataset, colourCols=NULL,
                              legendTitles=NULL, xLabels=NULL, legendColNums=NULL,
                              arrangeModes=NULL, overallTitle=""){

  # Test the length of metadata against the model we're gonna make

  numComponents = length(modelStabilityOutput[[1]])
  numModes = length(modelStabilityOutput)
  metaData = list(dataset$mode1, dataset$mode2, dataset$mode3)

  # Make a fake model object
  model = list()
  ymins = list()
  ymaxs = list()

  for(m in 1:numModes){
    medians = array(0L, dim=c(nrow(metaData[[m]]), numComponents))
    mins = array(0L, dim=c(nrow(metaData[[m]]), numComponents))
    maxs = array(0L, dim=c(nrow(metaData[[m]]), numComponents))

    for(f in 1:numComponents){
      medians[,f] = apply(modelStabilityOutput[[m]][[f]], 1, function(x){stats::median(x,na.rm=TRUE)})
      mins[,f] = medians[,f] - abs(apply(modelStabilityOutput[[m]][[f]], 1, function(x){stats::quantile(x,0.25,na.rm=TRUE)}))
      maxs[,f] = medians[,f] + abs(apply(modelStabilityOutput[[m]][[f]], 1, function(x){stats::quantile(x,0.75,na.rm=TRUE)}))
    }

    model[[m]] = cbind(medians, metaData[[m]]) %>% dplyr::as_tibble()
    ymins[[m]] = cbind(mins, metaData[[m]]) %>% dplyr::as_tibble()
    ymaxs[[m]] = cbind(maxs, metaData[[m]]) %>% dplyr::as_tibble()

    colnames(model[[m]]) = c(paste0("Component_", 1:numComponents), colnames(metaData[[m]]))
    colnames(ymins[[m]]) = c(paste0("Component_", 1:numComponents), colnames(metaData[[m]]))
    colnames(ymaxs[[m]]) = c(paste0("Component_", 1:numComponents), colnames(metaData[[m]]))

    if(colourCols[m] != ""){
      model[[m]] = model[[m]] %>% dplyr::arrange(!!dplyr::sym(colourCols[[m]]))
      ymins[[m]] = ymins[[m]] %>% dplyr::arrange(!!dplyr::sym(colourCols[[m]]))
      ymaxs[[m]] = ymaxs[[m]] %>% dplyr::arrange(!!dplyr::sym(colourCols[[m]]))
    }
  }

  # Make a regular model plot as if the medians are the loadings
  # No support for continuousModes because we need bar plots with error bars
  plotlist = plotPARAFACmodel(model, colourCols, legendTitles, xLabels, legendColNums, arrangeModes, continuousModes=rep(FALSE,numModes))

  # Iterate over the plots and add error bars
  plotIterator = 1
  for(f in 1:numComponents){
    for(m in 1:numModes){
      plot = plotlist[[plotIterator]]
      data = cbind(model[[m]][,f], ymins[[m]][,f], ymaxs[[m]][,f])
      colnames(data) = c("y", "ymin", "ymax")
      data = data %>% dplyr::as_tibble() %>% dplyr::mutate(index=dplyr::row_number())
      plot = plot + ggplot2::geom_errorbar(ggplot2::aes(x=index, ymin=ymin, ymax=ymax), width=0.2, data=data, inherit.aes = FALSE)
      plotlist[[plotIterator]] = plot
      plotIterator = plotIterator + 1
    }
  }

  # Add overall title
  outputPlot = ggpubr::ggarrange(plotlist=plotlist, nrow=numComponents+1, ncol=numModes)
  output = ggpubr::annotate_figure(outputPlot, top=ggpubr::text_grob(overallTitle))
  return(output)
}

# Fix namespace issues caused by ggplot2
ymin = NULL
ymax = NULL
