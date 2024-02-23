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
#' library(dplyr)
#' library(ggplot2)
#' X = Fujita2023$data
#' assessment = assessNumComponents(X, minNumComponents=1, maxNumComponents=3, numRepetitions=10)
#' plotModelMetric(assessment$metrics, "numIterations")
plotModelMetric = function(metrics, metricName="numIterations"){
  stopifnot(metricName %in% c("numIterations", "SSE", "CORCONDIA", "varExp"))

  # Override regular behaviour for TCC
  if(metricName == "TCC"){
    plot = plotModelTCCs(metrics$TCC)
    return(plot)
  }

  numModels = nrow(metrics[metricName][[1]])

  plot = metrics[metricName][[1]] %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(index=1:numModels) %>%
    tidyr::pivot_longer(-index) %>%
    ggplot2::ggplot(ggplot2::aes(x=index,y=value)) +
    ggplot2::facet_wrap(ggplot2::vars(name)) +
    ggplot2::geom_bar(stat="identity") +
    ggplot2::xlab("Model number")

  if(metricName == "numIterations"){
    plot = plot + ggplot2::ylab("Number of iterations")
  } else if(metricName == "SSE"){
    plot = plot + ggplot2::ylab("Sum of squared errors")
  } else if(metricName == "CORCONDIA"){
    plot = plot + ggplot2::ylab("CORCONDIA") + ggplot2::ylim(min(c(metrics[metricName][[1]],0)),100)
  } else if (metricName == "varExp"){
    plot = plot + ggplot2::ylab("Variance explained (%)") + ggplot2::ylim(0,100)
  }

  return(plot)
}

# Ugly solution to namespace issues caused by dplyr
index <- NULL
value <- NULL
name <- NULL
