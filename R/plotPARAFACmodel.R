#' Plot PARAFAC model
#'
#' @param model Model output from [parafac4microbiome::parafac()].
#' @param dataset The input dataset used to create the model.
#' @param colourCols Vector of strings stating which column names should be factorized for colours per mode.
#' @param legendTitles Vector of strings stating the legend title per mode.
#' @param xLabels Vector of strings stating the x-axis labels per mode.
#' @param legendColNums Vector of integers stating the desired number of columns for the legends per mode.
#' @param arrangeModes Vector of boolean values per mode, stating if the loadings should be arranged according to colourCols (TRUE) or not (FALSE).
#' @param continuousModes Vector of boolean values per mode, stating if the loadings should be plotted as a line plot (TRUE) or a bar plot (FALSE).
#' @param overallTitle Overall title of the plot.
#'
#' @return Plot object
#' @export
#'
#' @examples
#' library(multiway)
#' library(dplyr)
#' library(ggplot2)
#' set.seed(0)
#'
#' # Make PARAFAC model
#' processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, centerMode=1, scaleMode=2)
#' model = parafac(processedFujita$data, nfac=3, nstart=100, verbose=FALSE)
#'
#' # Make plot
#' plotPARAFACmodel(model, processedFujita,
#'   colourCols = c("", "Genus", ""),
#'   legendTitles = c("", "Genus", ""),
#'   xLabels = c("Replicate", "Feature index", "Time point"),
#'   legendColNums = c(0,5,0),
#'   arrangeModes = c(FALSE, TRUE, FALSE),
#'   continuousModes = c(FALSE,FALSE,TRUE),
#'   overallTitle = "Fujita PARAFAC model")
#'
plotPARAFACmodel = function(model, dataset, colourCols=NULL, legendTitles=NULL, xLabels=NULL, legendColNums=NULL, arrangeModes=NULL, continuousModes=NULL, overallTitle=""){
  if(methods::is(model, "parafac")){
    model = convertModelFormat(model, list(dataset$mode1, dataset$mode2, dataset$mode3))
  }

  stopifnot(methods::is(model,"list"))

  numModes = length(model)

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

  # Check if every vector has length equal to number of modes
  if(length(colourCols) != numModes){
    warning("colourCols length does not match the number of modes in the model. Default setting will be used.")
    colourCols = rep("", numModes)
  }
  if(length(legendTitles) != numModes){
    warning("legendTitles length does not match the number of modes in the model. Default setting will be used.")
    legendTitles = rep("", numModes)
  }
  if(length(xLabels) != numModes){
    warning("xLabels length does not match the number of modes in the model. Default setting will be used.")
    xLabels = rep("", numModes)
  }
  if(length(legendColNums) != numModes){
    warning("legendColNums length does not match the number of modes in the model. Default setting will be used.")
    legendColNums = rep(0, numModes)
  }
  if(length(arrangeModes) != numModes){
    warning("arrangeModes length does not match the number of modes in the model. Default setting will be used.")
    arrangeModes = rep(FALSE, numModes)
  }
  if(length(continuousModes) != numModes){
    warning("continuousModes length does not match the number of modes in the model. Default setting will be used.")
    continuousModes = rep(FALSE, numModes)
  }

  # Check if every vector is of the right type
  if(!methods::is(colourCols, "character")){
    warning("colourCols type is incorrect. Default setting will be used.")
    colourCols = rep("", numModes)
  }
  if(!methods::is(legendTitles, "character")){
    warning("legendTitles type is incorrect. Default setting will be used.")
    legendTitles = rep("", numModes)
  }
  if(!methods::is(xLabels, "character")){
    warning("xLabels type is incorrect. Default setting will be used.")
    xLabels = rep("", numModes)
  }
  if(!methods::is(legendColNums, "numeric")){
    warning("legendColNums type is incorrect. Default setting will be used.")
    legendColNums = rep(0, numModes)
  }
  if(!methods::is(arrangeModes, "logical")){
    warning("arrangeModes type is incorrect. Default setting will be used.")
    arrangeModes = rep(FALSE, numModes)
  }
  if(!methods::is(continuousModes, "logical")){
    warning("continuousModes type is incorrect. Default setting will be used.")
    continuousModes = rep(FALSE, numModes)
  }

  numComponents = ncol(model[[1]] %>% dplyr::select(dplyr::all_of(dplyr::starts_with("Component_"))))
  plotList = list()
  legends = list()
  empty = ggplot2::ggplot() + ggplot2::theme_void()

  plotIterator = 1

  for (n in 1:numComponents) {
    for (m in 1:numModes){

      componentCol = paste0("Component_", n)
      colourCol = colourCols[m]
      arrangeMode = arrangeModes[m]
      continuousMode = continuousModes[m]

      if(colourCol != "" & colourCol %in% colnames(model[[m]])){
        plot = model[[m]] %>%
          dplyr::select(dplyr::all_of(c(componentCol, colourCol))) %>%
          dplyr::arrange(!!dplyr::sym(colourCol)) %>%
          dplyr::mutate(index=dplyr::row_number()) %>%
          ggplot2::ggplot(ggplot2::aes(x=index,y=!!dplyr::sym(componentCol),fill=as.factor(!!dplyr::sym(colourCol))))
      }
      else{
        plot = model[[m]] %>%
          dplyr::select(dplyr::all_of(componentCol)) %>%
          dplyr::mutate(index=dplyr::row_number()) %>%
          ggplot2::ggplot(ggplot2::aes(x=index,y=!!dplyr::sym(componentCol)))
      }

      if(continuousMode == TRUE){
        plot = plot + ggplot2::geom_line()
      }
      else{
        plot = plot + ggplot2::geom_bar(stat="identity") # col="black" was here
      }

      if(m==1){
        plot = plot + ggplot2::ylab(paste0("Component ", n))
      }
      else{
        plot = plot + ggplot2::ylab("")
      }

      if(n==numComponents){
        plot = plot + ggplot2::xlab(xLabels[m])
      }
      else{
        plot = plot + ggplot2::xlab("")
      }

      if(n==1 & colourCol != "" & colourCol %in% colnames(model[[m]])){
        legends[[m]] = cowplot::get_legend(plot +
                                    ggplot2::scale_fill_discrete(legendTitles[m]) +
                                    ggplot2::guides(fill = ggplot2::guide_legend(ncol=legendColNums[m])))
      }

      plotList[[plotIterator]] = plot + ggplot2::theme(legend.position="none")
      plotIterator = plotIterator + 1
    }
  }

  for(m in 1:numModes){
    if (colourCols[m] != "" & colourCols[m] %in% colnames(model[[m]])){
      plotList[[plotIterator]] = legends[[m]]
    }
    else{
      plotList[[plotIterator]] = empty
    }
    plotIterator = plotIterator + 1
  }

  outputPlot = ggpubr::ggarrange(plotlist = plotList, ncol=3, nrow=numComponents+1)

  if(overallTitle != ""){
    print(ggpubr::annotate_figure(outputPlot, top=ggpubr::text_grob(overallTitle)))
  }
  else{
    return(plotList)
  }
}
