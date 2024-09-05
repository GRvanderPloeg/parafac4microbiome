#' Plots diagnostics for selecting the number of components of a parafac model.
#'
#' @param numIterations Matrix of number of iterations needed per initialized model (number of models x number of components).
#' @param SSE Sum of squared errors of the initialized models (number of models x number of components).
#' @param CORCONDIA CORCONDIA scores of the initialized models (see [corcondia]) (number of models x number of components).
#' @param varExp Variation explained by the initialized models (number of models x number of components).
#' @param TCC Cube of Tucker Congruence Coefficients comparing the loadings between two components within a mode (mode x component x component x model).
#'
#' @return An overview plot as well as plots of the individual metrics.
#' @export
#'
#' @examples
#' numIterations = array(round(runif(100*2)*100), c(100, 2))
#' SSE = array(rnorm(100*2, mean=1e4, sd=100), c(100, 2))
#' CORCONDIA = array(runif(100*2, min=90, max=100), c(100,2))
#' varExp = array(runif(100*2, min=50, max=100), c(100,2))
#' TCC = list(NULL, array(rnorm(3*2*2*100), c(3,2,2,100)))
#' plots = plotModelMetric(numIterations, SSE, CORCONDIA, varExp, TCC)
#' plots$overview
#' plots$numIterations
#' plots$SSE
#' plots$CORCONDIA
#' plots$varExp
plotModelMetric = function(numIterations, SSE, CORCONDIA, varExp, TCC){

  metrics = list("numIterations"=as.matrix(numIterations),
                 "SSE"=as.matrix(SSE),
                 "CORCONDIA"=as.matrix(CORCONDIA),
                 "varExp"=as.matrix(varExp))

  output = list()
  overviewPlotlist = list()
  numModels = nrow(metrics[[1]])
  numComponents = ncol(metrics[[1]])

  # Create and store a bar and box plot of every metric
  metricNames = c("numIterations", "SSE", "CORCONDIA", "varExp")
  for(i in 1:length(metricNames)){
    metricName = metricNames[i]

    overviewBox = metrics[metricName][[1]] %>%
      as.data.frame() %>%
      dplyr::as_tibble() %>%
      tidyr::pivot_longer(dplyr::everything()) %>%
      ggplot2::ggplot(ggplot2::aes(x=as.factor(name),y=value)) +
      ggplot2::geom_boxplot() +
      ggplot2::xlab("Number of components")

    overviewBar = metrics[metricName][[1]] %>%
      as.data.frame() %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(index=1:numModels) %>%
      tidyr::pivot_longer(-index) %>%
      ggplot2::ggplot(ggplot2::aes(x=index,y=value)) +
      ggplot2::facet_wrap(ggplot2::vars(name)) +
      ggplot2::geom_bar(stat="identity") +
      ggplot2::xlab("Model index")

    if(metricName == "numIterations"){
      overviewBox = overviewBox + ggplot2::ylab("Number of iterations")
      overviewBar = overviewBar + ggplot2::ylab("Number of iterations")
      output$numIterations = overviewBar
    } else if(metricName == "SSE"){
      overviewBox = overviewBox + ggplot2::ylab("Sum of squared errors")
      overviewBar = overviewBar + ggplot2::ylab("Sum of squared errors")
      output$SSE = overviewBar
    } else if(metricName == "CORCONDIA"){
      overviewBox = overviewBox + ggplot2::ylab("CORCONDIA")
      overviewBar = overviewBar + ggplot2::ylab("CORCONDIA")
      output$CORCONDIA = overviewBar
    } else if (metricName == "varExp"){
      overviewBox = overviewBox + ggplot2::ylab("Variance explained (%)") + ggplot2::ylim(0,100)
      overviewBar = overviewBar + ggplot2::ylab("Variance explained (%)") + ggplot2::ylim(0,100)
      output$varExp = overviewBar
    }

    overviewPlotlist[[i]] = overviewBox
  }

  # Plot TCC separately
  output$TCC = list()
  output$TCCoverall = list()
  for(i in 2:numComponents){
    output$TCC[[i]] = plotModelTCCs(TCC[[i]])
    output$TCCoverall[[i]] = plotOverallTCCs(TCC[[i]])
  }

  # Add overview boxplot
  output$overview = ggpubr::ggarrange(plotlist=overviewPlotlist)
  return(output)
}

# Ugly solution to namespace issues caused by dplyr
index <- NULL
value <- NULL
name <- NULL
