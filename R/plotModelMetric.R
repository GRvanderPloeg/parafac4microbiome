#' Title
#'
#' @param metrics Matrics object created by [assessNumComponents()].
#' @param metricName Name of the metric to inspect, one of:
#' * numIterations: the number of iterations needed to create the models (default).
#' * SSE: the sum of squared error.
#' * CORCONDIA: the core consistency diagnostic.
#' * varExp: the variance explained (%)
#'
#' @return Plot object
#' @export
#'
#' @examples
#' X = Fujita2023$data
#' assessment = assessNumComponents(X, minNumComponents=1, maxNumComponents=3, numRepetitions=10)
#' plotModelMetric(assessment$metrics, "numIterations")
plotModelMetric = function(metrics, metricName="numIterations"){
  stopifnot(metricName %in% c("numIterations", "SSE", "CORCONDIA", "varExp"))

  plot = metrics[metricName][[1]] %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(index=1:nrow(.)) %>%
    tidyr::pivot_longer(-index) %>%
    ggplot2::ggplot(aes(x=index,y=value)) +
    ggplot2::facet_wrap(vars(name)) +
    ggplot2::geom_bar(stat="identity") +
    ggplot2::xlab("Model number")

  if(metricName == "numIterations"){
    plot = plot + ggplot2::ylab("Number of iterations")
  } else if(metricName == "SSE"){
    plot = plot + ggplot2::ylab("Sum of squared errors")
  } else if(metricName == "CORCONDIA"){
    plot = plot + ggplot2::ylab("CORCONDIA") + ggplot2::ylim(min(metrics[metricName][[1]]),100)
  } else if (metricName == "varExp"){
    plot = plot + ggplot2::ylab("Variance explained (%)") + ggplot2::ylim(0,100)
  }

  return(plot)
}

