#' Plot model stability check output.
#'
#' @param modelStabilityOutput Outout of the [modelStabilityCheck()] function.
#'
#' @return Plot
#' @export
#'
#' @examples
#' library(dplyr)
#' library(ggplot2)
#' library(ggpubr)
#' modelStability = modelStabilityCheck(Fujita2023$data, Fujita2023$sampleMetadata, numComponents=3)
#' plotModelStability(modelStability)
plotModelStability = function(modelStabilityOutput){
  numComponents = length(modelStabilityOutput[[1]])
  numModes = length(modelStabilityOutput)

  plotlist = list()
  plotIterator = 1
  for(f in 1:numComponents){
    for(m in 1:numModes){
      df = modelStabilityOutput[[m]][[f]]
      df = cbind(apply(df, 1, stats::median, na.rm=TRUE),
                 apply(df, 1, function(x){stats::quantile(x, 0.25, na.rm=TRUE)}),
                 apply(df, 1, function(x){stats::quantile(x, 0.75, na.rm=TRUE)}))
      colnames(df) = c("y", "minValue", "maxValue")

      plotlist[[plotIterator]] = df %>%
        dplyr::as_tibble() %>%
        dplyr::mutate(index=1:nrow(df)) %>%
        ggplot2::ggplot(ggplot2::aes(x=index,y=y)) +
        ggplot2::geom_bar(stat="identity") +
        ggplot2::geom_errorbar(ggplot2::aes(ymin=minValue,ymax=maxValue))
      plotIterator = plotIterator + 1
    }
  }

  plot = ggpubr::ggarrange(plotlist=plotlist)
  return(plot)
}

# Ugly solution for dplyr namespace issues
index <- NULL
y <- NULL
minValue <- NULL
maxValue <- NULL
