#' Plot a summary of the loadings of many initialized parafac models.
#'
#' @inheritParams plotPARAFACmodel
#' @param models Models list output from [parafac()] using output="all".
#'
#' @return Plot of loadings with error bars
#' @export
#'
#' @examples
#' processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, centerMode=1, scaleMode=2)
#' models = parafac(processedFujita$data, 2, nstart=10, output="all")
#' plotModelStability(models, processedFujita)
plotModelStability = function(models, dataset, colourCols=NULL,
                              legendTitles=NULL, xLabels=NULL, legendColNums=NULL,
                              arrangeModes=NULL, continuousModes=NULL, overallTitle=""){

  numComponents = ncol(models[[1]]$Fac[[1]])
  numModes = length(models[[1]]$Fac)

  # Separate out all A, B, C loadings per component into one matrix per combination for easy plotting
  As = list()
  Bs = list()
  Cs = list()
  for(i in 1:numComponents){
    As[[i]] = simplify2array(lapply(models, function(x){x$Fac[[1]][,i]}))
    Bs[[i]] = simplify2array(lapply(models, function(x){x$Fac[[2]][,i]}))
    Cs[[i]] = simplify2array(lapply(models, function(x){x$Fac[[3]][,i]}))
  }

  # Test the length of metadata against the model we're gonna make
  modelStabilityOutput = list(As, Bs, Cs)
  metaData = list(dataset$mode1, dataset$mode2, dataset$mode3)

  # Convert default settings to usable content.
  if(is.null(colourCols)){
    colourCols = rep("", numModes)
  }
  if(is.null(legendTitles)){
    legendTitles = rep("", numModes)
  }
  if(is.null(xLabels)){
    xLabels = rep("", numModes)
  }
  if(is.null(legendColNums)){
    legendColNums = rep(0, numModes)
  }
  if(is.null(arrangeModes)){
    arrangeModes = rep(FALSE, numModes)
  }
  if(is.null(continuousModes)){
    continuousModes = rep(FALSE, numModes)
  }

  # Make a fake model object
  model = list()
  ymins = list()
  ymaxs = list()
  metadata = list(dataset$mode1, dataset$mode2, dataset$mode3)

  for(m in 1:numModes){
    medians = array(0L, dim=c(nrow(metaData[[m]]), numComponents))
    mins = array(0L, dim=c(nrow(metaData[[m]]), numComponents))
    maxs = array(0L, dim=c(nrow(metaData[[m]]), numComponents))

    for(f in 1:numComponents){
      medians[,f] = apply(modelStabilityOutput[[m]][[f]], 1, function(x){stats::median(x,na.rm=TRUE)})
      mins[,f] = apply(modelStabilityOutput[[m]][[f]], 1, function(x){stats::quantile(x,0.25,na.rm=TRUE)})
      maxs[,f] = apply(modelStabilityOutput[[m]][[f]], 1, function(x){stats::quantile(x,0.75,na.rm=TRUE)})
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

      # Change dataset mode metadata order to fit with the fake model
      metadata[[m]] = metadata[[m]] %>% dplyr::arrange(!!dplyr::sym(colourCols[[m]]))
    }
  }

  # Make a regular model plot as if the medians are the loadings
  # No support for continuousModes because we need bar plots with error bars
  dataset$mode1 = metadata[[1]]
  dataset$mode2 = metadata[[2]]
  dataset$mode3 = metadata[[3]]
  plotlist = plotPARAFACmodel(model, dataset, numComponents, colourCols, legendTitles, xLabels, legendColNums, arrangeModes, continuousModes)

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
