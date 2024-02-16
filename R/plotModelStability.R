#' Plot model stability check output.
#'
#' @param modelStabilityOutput Outout of the [modelStabilityCheck()] function.
#'
#' @return Plot
#' @export
#'
#' @examples
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
      df = cbind(apply(df, 1, mean, na.rm=TRUE), apply(df, 1, sd, na.rm=TRUE))
      colnames(df) = c("m", "s")
      plotlist[[plotIterator]] = df %>%
        as_tibble() %>%
        mutate(index=1:nrow(.)) %>%
        ggplot(aes(x=index,y=m)) +
        geom_bar(stat="identity") +
        geom_errorbar(aes(ymin=m-s,ymax=m+s))
      plotIterator = plotIterator + 1
    }
  }

  plot = ggarrange(plotlist=plotlist)
  return(plot)
}
