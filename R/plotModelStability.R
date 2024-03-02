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
#' modelStability = modelStabilityCheck(Fujita2023$data, Fujita2023$sampleMetadata, numComponents=3)
#'
#' plotModelStability(modelStability,
#'  dataset=processedFujita,
#'  colourCols=c("","Phylum", ""),
#'  legendTitles=c("","Phylum",""),
#'  xLabels=c("Replicate", "Feature index", "Time index"),
#'  legendColNums=c(0,3,0),
#'  arrangeModes=c(FALSE,TRUE,FALSE),
#'  continuousModes=c(FALSE,FALSE,FALSE),
#'  overallTitle="Fujita2023")
#'
plotModelStability = function(modelStabilityOutput, dataset, colourCols=NULL,
                              legendTitles=NULL, xLabels=NULL, legendColNums=NULL,
                              arrangeModes=NULL, continuousModes=NULL, overallTitle=""){

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
      medians[,f] = apply(modelStabilityOutput[[m]][[f]], 1, function(x){median(x,na.rm=TRUE)})
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
      model[[m]] = model[[m]] %>% arrange(!!dplyr::sym(colourCols[[m]]))
      ymins[[m]] = ymins[[m]] %>% arrange(!!dplyr::sym(colourCols[[m]]))
      ymaxs[[m]] = ymaxs[[m]] %>% arrange(!!dplyr::sym(colourCols[[m]]))
    }
  }

  # Give it to plotPARAFACmodel
  plotlist = plotPARAFACmodel(model, colourCols, legendTitles, xLabels, legendColNums, arrangeModes, continuousModes)

  # order of plotlist: iterate over modes first, then components
  # Step in between: you need to sort the values by the colourCol

  # Grab the plots from there and manually add the error bars

  plotIterator = 1
  for(f in 1:numComponents){
    for(m in 1:numModes){
      plot = plotlist[[plotIterator]]
      data = cbind(model[[m]][,f], ymins[[m]][,f], ymaxs[[m]][,f])
      colnames(data) = c("y", "ymin", "ymax")
      data = data %>% as_tibble() %>% mutate(index=1:nrow(.))
      plot = plot + geom_errorbar(aes(x=index, ymin=ymin, ymax=ymax), width=0.2, data=data, inherit.aes = FALSE)
      plotlist[[plotIterator]] = plot
      plotIterator = plotIterator + 1
    }
  }

  # numComponents = length(modelStabilityOutput[[1]])
  # numModes = length(modelStabilityOutput)
  # metaData = list(dataset$mode1, dataset$mode2, dataset$mode3)
  #
  # # Convert default settings to usable content.
  # if(is.null(colourCols)){
  #   colourCols = rep("", numModes)
  # }
  # if(is.null(legendTitles)){
  #   legendTitles = rep("", numModes)
  # }
  # if(is.null(xLabels)){
  #   xLabels = rep("", numModes)
  # }
  # if(is.null(legendColNums)){
  #   legendColNums = rep(0, numModes)
  # }
  # if(is.null(arrangeModes)){
  #   arrangeModes = rep(FALSE, numModes)
  # }
  # if(is.null(continuousModes)){
  #   continuousModes = rep(FALSE, numModes)
  # }
  #
  # # Check if every vector has length equal to number of modes
  # if(length(colourCols) != numModes){
  #   warning("colourCols length does not match the number of modes in the model. Default setting will be used.")
  #   colourCols = rep("", numModes)
  # }
  # if(length(legendTitles) != numModes){
  #   warning("legendTitles length does not match the number of modes in the model. Default setting will be used.")
  #   legendTitles = rep("", numModes)
  # }
  # if(length(xLabels) != numModes){
  #   warning("xLabels length does not match the number of modes in the model. Default setting will be used.")
  #   xLabels = rep("", numModes)
  # }
  # if(length(legendColNums) != numModes){
  #   warning("legendColNums length does not match the number of modes in the model. Default setting will be used.")
  #   legendColNums = rep(0, numModes)
  # }
  # if(length(arrangeModes) != numModes){
  #   warning("arrangeModes length does not match the number of modes in the model. Default setting will be used.")
  #   arrangeModes = rep(FALSE, numModes)
  # }
  # if(length(continuousModes) != numModes){
  #   warning("continuousModes length does not match the number of modes in the model. Default setting will be used.")
  #   continuousModes = rep(FALSE, numModes)
  # }
  #
  # # Check if every vector is of the right type
  # if(!methods::is(colourCols, "character")){
  #   warning("colourCols type is incorrect. Default setting will be used.")
  #   colourCols = rep("", numModes)
  # }
  # if(!methods::is(legendTitles, "character")){
  #   warning("legendTitles type is incorrect. Default setting will be used.")
  #   legendTitles = rep("", numModes)
  # }
  # if(!methods::is(xLabels, "character")){
  #   warning("xLabels type is incorrect. Default setting will be used.")
  #   xLabels = rep("", numModes)
  # }
  # if(!methods::is(legendColNums, "numeric")){
  #   warning("legendColNums type is incorrect. Default setting will be used.")
  #   legendColNums = rep(0, numModes)
  # }
  # if(!methods::is(arrangeModes, "logical")){
  #   warning("arrangeModes type is incorrect. Default setting will be used.")
  #   arrangeModes = rep(FALSE, numModes)
  # }
  # if(!methods::is(continuousModes, "logical")){
  #   warning("continuousModes type is incorrect. Default setting will be used.")
  #   continuousModes = rep(FALSE, numModes)
  # }
  #
  # plotlist = list()
  # plotIterator = 1
  # for(n in 1:numComponents){
  #   for(m in 1:numModes){
  #
  #     componentCol = paste0("Component_", n)
  #     colourCol = colourCols[m]
  #     arrangeMode = arrangeModes[m]
  #     continuousMode = continuousModes[m]
  #
  #     df = modelStabilityOutput[[m]][[n]]
  #     df = cbind(apply(df, 1, stats::median, na.rm=TRUE),
  #                apply(df, 1, function(x){stats::quantile(x, 0.25, na.rm=TRUE)}),
  #                apply(df, 1, function(x){stats::quantile(x, 0.75, na.rm=TRUE)}))
  #     colnames(df) = c("y", "minValue", "maxValue")
  #
  #     if(colourCol != "" & colourCol %in% colnames(metaData[[m]])){
  #       plot = cbind(df, metaData[[m]]) %>%
  #         dplyr::as_tibble() %>%
  #         dplyr::arrange(!!dplyr::sym(colourCol)) %>%
  #         dplyr::mutate(index=1:nrow(df)) %>%
  #         ggplot2::ggplot(ggplot2::aes(x=index,y=y,fill=as.factor(colourCol))) +
  #         ggplot2::geom_bar(stat="identity") +
  #         ggplot2::geom_errorbar(ggplot2::aes(ymin=minValue,ymax=maxValue))
  #     }
  #     else{
  #       plot = df %>%
  #         dplyr::as_tibble() %>%
  #         dplyr::mutate(index=1:nrow(df)) %>%
  #         ggplot2::ggplot(ggplot2::aes(x=index,y=y)) +
  #         ggplot2::geom_bar(stat="identity") +
  #         ggplot2::geom_errorbar(ggplot2::aes(ymin=minValue,ymax=maxValue))
  #     }
  #
  #     if(continuousMode == TRUE){
  #       plot = plot + ggplot2::geom_line()
  #     }
  #     else{
  #       plot = plot + ggplot2::geom_bar(stat="identity") # col="black" was here
  #     }
  #
  #     if(m==1){
  #       plot = plot + ggplot2::ylab(paste0("Component ", n))
  #     }
  #     else{
  #       plot = plot + ggplot2::ylab("")
  #     }
  #
  #     if(n==numComponents){
  #       plot = plot + ggplot2::xlab(xLabels[m])
  #     }
  #     else{
  #       plot = plot + ggplot2::xlab("")
  #     }
  #
  #     plotlist[[plotIterator]] = plot
  #     plotIterator = plotIterator + 1
  #   }
  # }

  plot = ggpubr::ggarrange(plotlist=plotlist, nrow=numComponents+1, ncol=numModes)
  return(plot)
}

# # Ugly solution for dplyr namespace issues
# index <- NULL
# y <- NULL
# minValue <- NULL
# maxValue <- NULL
