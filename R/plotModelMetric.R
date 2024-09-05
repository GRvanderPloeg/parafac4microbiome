#' Plot diagnostics of many initialized PARAFAC models.
#'
#' @param metric Matrix of metrics per initialized model (number of models x number of components).
#' @param plottingMode Plot the metrics as a box plot ("box", default) or as a bar plot ("bar").
#' @param ylabel String of the y axis label (default "metric").
#' @param titleString String of the plot title (default "").
# '
#' @return A plot of the metrics
#' @export
#'
#' @examples
#' varExp = array(runif(100*2, min=50, max=100), c(100,2))
#' plotModelMetric(varExp, plottingMode="box", ylabel="Variation explained (%)")
plotModelMetric = function(metric, plottingMode="box", ylabel="metric", titleString=""){

  if(!methods::is(metric, "data.frame")){
    metric = as.data.frame(metric)
    colnames(metric) = 1:ncol(metric)
  }
  numModels = nrow(metric)
  numComponents = ncol(metric)

  if(plottingMode == "box"){
    plot = metric %>%
      dplyr::as_tibble() %>%
      tidyr::pivot_longer(dplyr::everything()) %>%
      ggplot2::ggplot(ggplot2::aes(x=as.factor(name),y=value)) +
      ggplot2::geom_boxplot() +
      ggplot2::xlab("Number of components") +
      ggplot2::ylab(ylabel) +
      ggplot2::ggtitle(titleString)

  } else if(plottingMode == "bar"){
    plot = metric %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(index=1:numModels) %>%
      tidyr::pivot_longer(-index) %>%
      ggplot2::ggplot(ggplot2::aes(x=index,y=value)) +
      ggplot2::facet_wrap(ggplot2::vars(name)) +
      ggplot2::geom_bar(stat="identity") +
      ggplot2::xlab("Model number") +
      ggplot2::ylab(ylabel) +
      ggplot2::ggtitle(titleString)
  }

  return(plot)
}

# Ugly solution to namespace issues caused by dplyr
index <- NULL
value <- NULL
name <- NULL
